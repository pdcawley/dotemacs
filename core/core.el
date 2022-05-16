;;; core.el --- Support stuff -*- lexical-binding: t; -*-

(when (< emacs-major-version 27)
  (error "Detected Emacs %s. Minimum supported version is 27.1."
	 emacs-version))

;; Ensure our core libraries are loadable
(add-to-list 'load-path (file-name-directory load-file-name))

;; Remember these variables initial values, so we can safely reset them at a
;; later time, or consul them without fear of contamination
(dolist (var '(exec-path load-path process-environment))
  (unless (get var 'initial-value)
    (put var 'initial-value (default-value var))))

;; Pulling those shoelaces
(require 'core-lib)

;;
;;; Initialize internal state

(defgroup pdc nil
  "Piers's over engineered emacs config.")

(defvar pdc-debug-p (or (getenv-internal "DEBUG") init-file-debug)
  "If non-nil, we might log more.

Use `pdc-debug-mode' to toggle it. The --debug-init flag and setting the DEBUG envvar will enable this at startup.")

(defvar pdc-init-p nil
  "Non-nil once we're initialized.")

(defvar pdc-init-time nil
  "The time it took, in seconds to initialize")

(defconst pdc-interactive-p (not noninteractive)
  "If non-nil, Emacs is in interactive mode.")

(defconst EMACS28+ (> emacs-major-version 27))
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD   (or IS-MAC (eq system-type 'berkeley-unix)))

;;
;;; Directory variables

(defconst pdc-emacs-dir user-emacs-directory
  "The path to the currently loaded .emcas.d directory. Must end with a slash.")

(defconst pdc-core-dir (concat pdc-emacs-dir "core/")
  "The root directory of our core files. Must end with a slash")

(defconst pdc-modules-dir (concat pdc-emacs-dir "modules/")
  "The root directory for our modules directory, if we ever need one. Ends with a /.")

(defconst pdc-local-dir
  (if-let (localdir (getenv-internal "PDCMACSLOCALDIR"))
      (expand-file-name (file-name-as-directroy localdir))
    (concat pdc-emacs-dir ".local/"))
  "Root directory for local storage.

Use this as a storage location for this system's installation.

These files should not be shared across systems. By default it is used by `pdc-etc-dir' and `pdc-cache-dir'. Must end with a slash.")

(defconst pdc-etc-dir (concat pdc-local-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files htat don't change much, like server binaries, external dependencies or long term shared data. Must end with a slash.")

(defconst pdc-cache-dir (concat pdc-local-dir "cache/")
  "Directory for volatile local storage.

Use this for files that change often. Must end with a slash.")

(defconst pdc-autoloads-file
  (concat pdc-local-dir "autoloads." emacs-version ".el")
  "Where `pdc-reload-core-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of our autoloaded core functions (in core/autoload/*.el).")

(defconst pdc-env-file (concat pdc-local-dir "env")
  "The location of our envvar file.

This files contains environment variables scraped from our shell environment which is loaded at startup (if it exists). This is helpful on annoying OSes like MacOS that don't necessarily set the right environment variables by default")

(defconst pdc-private-dir
  (if-let (pdcdir (getenv-internal "PDCMACSDIR"))
      (expand-file-name (file-name-as-directory pdcdir))
    (or (let ((xdgdir
               (expand-file-name "pdcmacs/"
                                 (or (getenv-internal "XDG_CONFIG_HOME")
                                     "~/.config"))))
          (if (file-directory-p xdgdir) xdgdir))
        "~/.pdcmacs.d/"))
  "Where your private configuration is placed.")

;;
;;; Custom error types

(define-error 'pdc-error "Error in our core libs")
(define-error 'pdc-hook-error "Error in a startup hook" 'pdc-error)
(define-error 'pdc-autoload-error "Error in our autoloads file." 'pdc-error)
(define-error 'pdc-module-error "Error in a module." 'pdc-error)
(define-error 'pdc-private-error "Error in private config." 'pdc-error)
(define-error 'pdc-package-error "Error with packages" 'pdc-error)

;;
;;; Custom hooks
(defvar pdc-after-reload-hook nil
  "A list of hooks ot run after `pdc/reload' has reloaded Emacs.")

(defvar pdc-before-reload-hook nil
  "A list of hooks to run before `pdc/reload' has reloaded Emacs.")

;;
;;; Native compilation support

(when EMACS28+
  (mapc (lambda (varset)
	  (unless (boundp (car varset))
	    (defvaralias (car varset) (cdr varset))))
	'((native-comp-deferred-compilation . comp-deferred-compilation)
          (native-comp-deferred-compilation-deny-list . comp-deferred-compilation-deny-list)
          (native-comp-eln-load-path . comp-eln-load-path)
          (native-comp-warning-on-missing-source . comp-warning-on-missing-source)
          (native-comp-driver-options . comp-native-driver-options)
          (native-comp-async-query-on-exit . comp-async-query-on-exit)
          (native-comp-async-report-warnings-errors . comp-async-report-warnings-errors)
          (native-comp-async-env-modifier-form . comp-async-env-modifier-form)
          (native-comp-async-all-done-hook . comp-async-all-done-hook)
          (native-comp-async-cu-done-functions . comp-async-cu-done-functions)
          (native-comp-async-jobs-number . comp-async-jobs-number)
          (native-comp-never-optimize-functions . comp-never-optimize-functions)
          (native-comp-bootstrap-deny-list . comp-bootstrap-deny-list)
          (native-comp-always-compile . comp-always-compile)
          (native-comp-verbose . comp-verbose)
          (native-comp-debug . comp-debug)
          (native-comp-speed . comp-speed)))

  ;; Don't store eln files in ~/.emacs.d/eln-cache
  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path (concat pdc-cache-dir "eln/"))))

(with-eval-after-load 'comp
  ;; HACK Disable native-compilation for some troublesome package
  (mapc (pdc-partial #'add-to-list 'native-comp-deferred-compilation-deny-list)
	(let ((local-dir-re (concat "\\`" (regexp-quote pdc-local-dir))))
	  (list (concat "\\`" (regexp-quote pdc-autoloads-file) "\\'")
		(concat local-dir-re ".*/evil-collection-vterm\\.el\\'")
		(concat local-dir-re ".*/with-editor\\.el\\'")
		(concat local-dir-re ".*/jupyter-channel\\.el\\'")))))

;;
;;; Don't litter our .emacs.d
(setq async-byte-compile-log-file (concat pdc-etc-dir "async-bytecomp.log")
      custom-file (concat pdc-private-dir "custom.el")
      desktop-dirname (concat pdc-etc-dir "desktop")
      desktop-base-file-name "autosave"
      desktop-base-lock-name "autosave-lock"
      pcache-directory (concat pdc-cache-dir "pcache/")
      request-storage-directory (concat pdc-cache-dir "request")
      shared-game-score-directory (concat pdc-etc-dir "shared-game-score/"))

(defadvice! pdc--write-tosane-paths-a (fn &rest args)
  "Write 3rd party files to `pdc-etc-dir' to keep `user-emacs-directory' clean.

Also writes `put' calls for saved safe-local-variables to `custom-file' instead of `user-init-file' (which `en/disable-command' in novice.el.gz is hardcoded to do)."
  :around #'en/disable-command
  :around #'locate-user-emacs-file
  (let ((user-emacs-directory pdc-etc-dir)
	(user-init-file custom-file))
    (apply fn args)))

;;
;;; Reasonable defaults

;; Let's at least attempt to be secure

(setq gnutls-verify-error (and (fboundp 'gnutls-available-p)
			       (gnutls-available-p)
			       (not (getenv-internal "INSECURE")))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
	(concat "SECURE128:+SECURE192:-VERS-ALL"
		(if (and (not IS-WINDOWS)
			 (>= libgnutls-version 30605))
		    "+VERS-TLS1.3")
		":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://ww.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
		    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
		    ;; compatibility fallbacks
		    "gnutls-cli -p %p %h"))

(setq auto-mode-case-fold nil)

(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)

(setq bidi-inhibit-bpa t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq ffap-machine-p-known 'reject)

(setq frame-inhibit-implied-resize t)

;; GC Tweaks
(use-package gcmh
  :config
    (setq gcmh-idle-delay 'auto)
    (setq gcmh-auto-idle-delay-factor 10)
    (setq gcmh-high-cons-threshold (* 16 1024 1024))
  :demand t
  :hook
  (emacs-startup-hook . gcmh-mode)
  :diminish "")

(add-hook 'emacs-startup-hook 'gcmh-mode) 

(setq idle-update-delay 1.0)

(setq inhibit-compacting-font-caches t)

(setq read-process-output-max (* 64 1024))

(setq redisplay-skip-fontification-on-input t)

(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
	w32-pipe-read-delay 0 		   ; faster IPC
	w32-pipe-buffer-size (* 64 1024))  ; read more at a time (was 4K)

  ;; Windows clipboard might have an odd encoding.
  (setq selection-coding-system 'utf-8))

(unless IS-MAC (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;;
;;; Reasonable defaults for interactive sessions

;; Disable warnings from legacy advice system. No bloody use and there's not much we can do about them anyway.
(setq ad-redefinition-action 'accept)

(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'lisp-interaction-mode
      initial-scratch-message nil)

(defun pdc-initialize (&optional force-p)
  "Bootstrap pdcmacs, if it hasn't already (or if FORCE-P is non-nil).

The bootstrap process ensures that everything Doom needs to run is set up;
essential directories exist, core packages are installed, `pdc-autoloads-file'
is laoded (failing if it isn't), that all teh needed hooks are in place.

The overall load order is as follows:

  ~/.emacs.d/early-init.el
  ~/.emacs.d/core/core.el
  ~/.emacs.d/init.el
  `after-init-hook'
  `emacs-startup-hook'
  `pdc-init-ui-hook'
  `window-setup-hook'"

  (when (or force-p (not pdc-init-p))
    (setq pdc-init-p t)
    (pdc-log "Initializing pdcmacs")

    ;; Reset as much state as possible so `pdc-initialize' can be treated like
    ;; a reset function. e.g. when loading the config
    (dolist (var '(exec-path load-path))
      (set-default var (get var 'initial-value)))

    ;; Get use-package etc loaded up
    (require 'core-modules)
    (pdc-intialize-modules)
    

    (when (or (display-graphic-p) (daemonp))
      (cond (pdc-env-file
	     (setq-default process-environment (get 'process-environment 'initial-value))
	     (pdc-load-envvars-file pdc-env-file 'noerror))
	    (t
	     (use-package exec-path-from-shell :init (exec-path-from-shell-initialize))))))
  
  pdc-init-p)

(provide 'core)
;;; core.el ends here
