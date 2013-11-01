;;(require 'eieio)
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))
;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))


(require 'autoinsert)
(require 'cl)
(require 'saveplace)
(require 'ffap)


(setq package-archives
      '(("gnu"       . "http://elpa.gnu.org/packages/")
        ("original"  . "http://tromey.com/elpa")
        ("org"       . "http://orgmode.org/elpa/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa"     . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defvar pdc/required-packages
  (list 'yasnippet 'use-package))

(let ((yas-bundle-desc (assq 'yasnippet-bundle package-alist)))
  (when yas-bundle-desc
    (package-delete "yasnippet-bundle"
                    (package-version-join
                     (package-desc-vers (cdr yas-bundle-desc))))))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package pdc/required-packages)
  (unless (package-installed-p package)
    (package-install package)))

(require 'use-package)

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

;;; Work around a bug on OS X where system-name is FQDN.
(if (or (eq system-type 'darwin)
        (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))



;;;_ , Utility macros and functions

(setq server-auth-dir (expand-file-name "~/server/"))
(setq-default gc-cons-threshold 10000000)

;; (mapc (lambda (p) (pushnew p exec-path))
;;       (reverse (list "~/bin" "~/local/node/bin" "~/perl5/perlbrew/bin"
;;                      "/usr/local/bin"
;;                      "/usr/local/sbin" "/sbin" "/bin" "/usr/bin" "/usr/sbin")))
;;(load custom-file)

(defun remove-extension (name)
             (string-match "\\(.*?\\)\.\\(org\\(\\.el\\)?\\|el\\)\\(\\.gpg\\)?$" name)
             (match-string 1 name))

;;; * Load User/System Specific Files
(cl-flet* ((sk-load-babel (file)
             (condition-case ()
                 (org-babel-load-file file)
               (error (message "Error while loading %s" file))))
           (sk-load-el (file)
             (condition-case ()
                 (load file)
               (error (message "Error while loading %s" file))))
           (sk-load (base)
             (let* ((path          (expand-file-name base user-emacs-directory))
                    (literate      (concat path ".org"))
                    (encrypted-org (concat path ".org.pg"))
                    (plain         (concat path ".el"))
                    (encrypted-el  (concat path ".el.gpg")))
               (cond
                ((file-exists-p encrypted-org) (sk-load-babel encrypted-org))
                ((file-exists-p encrypted-el)  (sk-load-el encrypted-el))
                ((file-exists-p literate)      (sk-load-babel literate))
                ((file-exists-p plain)         (sk-load-el plain)))))
           (remove-extension (name)
             (string-match "\\(.*?\\)\.\\(org\\(\\.el\\)?\\|el\\)\\(\\.gpg\\)?$" name)
             (match-string 1 name)))
      (let ((user-dir (expand-file-name user-login-name user-emacs-directory)))
        (dolist (default-directory (nreverse
                                    (list user-override-directory
                                          user-lisp-directory
                                          user-lib-directory
                                          user-site-lisp-directory
                                          user-external-lisp-directory)))
          (when (file-exists-p default-directory)
            (normal-top-level-add-subdirs-to-load-path)))
        (sk-load system-name)
        (sk-load user-login-name)
        (dolist (dir (list user-dir user-initscripts-directory))
          (when (file-exists-p dir)
            (add-to-list 'load-path dir)
            (mapc #'sk-load
                  (remove-duplicates
                   (mapcar #'remove-extension
                           (directory-files dir t ".*\.\\(org\\|el\\)\\(\\.gpg\\)?$"))
                   :test #'string=))))))


(setq custom-file (expand-file-name "preferences.el" user-emacs-directory))
(load-file custom-file)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
