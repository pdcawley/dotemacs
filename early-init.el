;;; early-init.el --- Setup straight/use-package -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)

(setq native-comp-deferred-compilation nil)

(setq package-enable-at-startup nil)

(setq load-prefer-newer noninteractive)
(setq straight-base-dir (expand-file-name ".local/" user-emacs-directory))
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name ".local/straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'diminish)
(straight-use-package 'which-key)
(straight-use-package 'no-littering)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; `file-name-handler-alist' is consulted on each `require', `load' and
    ;; various path/io functions. You get a minor speed up by unsetting this.
    (setq-default file-name-handler-alist nil)
    ;; ...but restore `file-name-handler-alist' later because it's needed for
    ;; handling encrypted or compressed files, among other things.
    (defun pdc-reset-file-handler-alist-h ()
      (setq file-name-handler-alist
	    ;; Merge don't overwrite because who knows what the rest of the
	    ;; startup process may have got up to
	    (delete-dups (append file-name-handler-alist
				 old-file-name-handler-alist))))
    (add-hook 'emacs-startup-hook #'pdc-reset-file-handler-alist-h 101))

  (setq-default inhibit-redisplay t
		inhibit-message t)
  (add-hook 'window-setup-hook
	    (lambda ()
	      (setq-default inhibit-redisplay nil
			    inhibit-message nil)
	      (redisplay))))

(set-language-environment "UTF-8")

(setq default-input-method nil)

;; Ensure pdcmacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(load (concat user-emacs-directory "core/core") nil 'nomessage)
