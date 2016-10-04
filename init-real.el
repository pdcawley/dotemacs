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

;; recompile configs

(add-hook 'kill-emacs-hook
          (lambda ()
            (byte-recompile-directory my-init-dir)))

;; elpa
(add-to-list 'package-archives '("marmalade" .
                                 "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(defconst my-init-dir (expand-file-name "initscripts" emacs-d))
(defconst emacs-major-version-rad 1000000)

(defun has-emacs-version (major minor)
  (<= (+ (* major emacs-major-version-rad) minor)
      (+ (* emacs-major-version emacs-major-version-rad) emacs-minor-version)))

(unless (has-emacs-version 24 0)
  (add-to-list 'package-archives '("gnu" . "http://elpa.org/packages")))

(package-initialize)

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
(use-package req-package
  :ensure t
  :config
  (req-package--log-set-level 'debug))


;; (require-package 'which-key)
;; (require 'which-key)
(req-package which-key
  :force t
  :diminish " Ⓚ"
  :config
  (let ((new-descriptions
         '(("select-window-\\(0-9\\)" . "window \\1")
           ("avy-goto-word-or-subword-1" . "avy word")
           ("shell-command" . "shell cmd")
           ("avy-goto-line" "avy line")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region"))))
    (dolist (nd new-descriptions)
      (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
            which-key-description-replacement-alist)))
  (setq which-key-special-keys nil
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.4)
  (which-key-mode))

(defvar leader-map (make-sparse-keymap))
(defvar leader-key "M-m")
(global-set-key (kbd leader-key) nil)
(req-package general
  :requires which-key
  :force t)

(defun bindings|expand-define-prefix (desc key docstr)
  (let* ((variable-name (intern (format "%s-leader-key" (symbol-name desc))))
         (doc (or docstr (symbol-name desc))))
    `(progn
       (defvar ,variable-name (format "%s %s" leader-key ,key)
         ,(format "Prefix for %s" doc))
       (general-define-key ,variable-name '(nil :which-key ,(symbol-name desc)))
       )))

(defmacro bindings|define-prefix (desc key &optional docstr)
  (declare (indent 1))
  (bindings|expand-define-prefix desc key docstr))


(bindings|define-prefix jump   "j" "jumping around")
(bindings|define-prefix search "s" "searching")
(bindings|define-prefix files  "f")
(bindings|define-prefix window "w" "windows")

;; init.d
(random t)
(req-package load-dir
  :force t
  :init
  (setq force-load-messages t
        load-dir-debug t
        load-dir-recursive t)
  :config
  (let ((load-path (cons my-init-dir load-path)))
    (load-dir-one my-init-dir)
    (load (expand-file-name system-name emacs-d) t)
    (load (expand-file-name user-login-name emacs-d) t)
    (req-package-finish)))

;; Everything that's going to mess with the server has finished, so:
(server-start)

(provide 'init-real)
