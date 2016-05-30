(setq message-log-max 16384)

;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load (expand-file-name "load-path" (file-name-directory user-init-file)))

(if (or (eq system-type 'darwin)
    (eq system-type 'berkeley-unix))
    (setq system-name (car (split-string system-name "\\."))))



;;;_ , Utility macros and functions

(setq server-auth-dir (expand-file-name "~/server/"))
(setq-default gc-cons-threshold 10000000)

(put 'dired-find-alternate-file 'disabled nil)

(require 'server)
(unless (server-running-p)
  (server-start))

;; recompile configs

(add-hook 'kill-emacs-hook
          (lambda ()
            (byte-recompile-directory my-init-dir)))

;; elpa
(add-to-list 'package-archives '("marmalade" .
                                 "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("sunrise" . "http://joseito.republika.pl/sunrise-commander/"))

(defconst my-init-dir (expand-file-name "initscripts" emacs-d))
(defconst emacs-major-version-rad 1000000)

(defun has-emacs-version (major minor)
  (<= (+ (* major emacs-major-version-rad) minor)
      (+ (* emacs-major-version emacs-major-version-rad) emacs-minor-version)))

(unless (has-emacs-version 24 0)
  (add-to-list 'package-archives '("gnu" . "http://elpa.org/packages")))

(eval-when-compile (package-initialize))

(defun require-package (package)
  "refresh package archives, check package presence and install if it's not installed"
  (unless (require package nil t)
    (let* ((ARCHIVES (progn (unless package-archive-contents
                              (package-refresh-contents))
                            package-archive-contents))
           (AVAIL (assoc package ARCHIVES)))
      (if AVAIL (package-install package)))
    (require package)))

;; el-get
(require-package 'use-package)
(require 'use-package)
(add-to-list 'load-path (expand-file-name "el-get/el-get" emacs-d))
(require-package 'el-get)
(use-package el-get
  :config
  (add-to-list 'el-get-recipe-path (expand-file-name "el-get/el-get/recipes" emacs-d))
  (el-get 'sync))


;; req-package
(require-package 'req-package)
(require 'req-package)
(req-package--log-set-level 'trace)


;; init.d
(random t)
(req-package load-dir
  :force true
  :init
  (setq force-load-messages t)
  (setq load-dir-debug t)
  (setq load-dir-recursive t)
  :config
  (load-dir-one my-init-dir)
  (req-package-finish)
  ;(funcall 'select-theme)
  )
