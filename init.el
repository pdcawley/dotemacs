;; init.el --- -*- lexical-binding: t; -*-

;;;
;;; Bootstrap some packages

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;;; user-emacs-directory structure

(defvar pdcmacs-config-file (expand-file-name "config.el"  user-emacs-directory)
  "Our post-init config file.")

(defvar pdcmacs-init-file (expand-file-name "init.el" user-emacs-directory))

(defvar pdcmacs-etc-directory (expand-file-name "etc/" user-emacs-directory)
  "Our etc/ directory.")
(defvar pdcmacs-var-directory (expand-file-name "var/" user-emacs-directory)
  "Our var/ directory.")

(mkdir pdcmacs-etc-directory t)
(mkdir pdcmacs-var-directory t)

;; Setup the coding system
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(set-clipboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;; Tweak custom-file's location
(setq custom-file (expand-file-name "preferences.el" pdcmacs-etc-directory))


;; Quiet a warning
(setopt large-file-warning-threshold (* 100 1000 1000))

(setq straight-use-package-by-default t)
(straight-use-package 'diminish)
(straight-use-package 'general)
(setq general-use-package-emit-autoloads t)
(require 'general-autoloads)
(or (require 'use-package nil t)
    (straight-use-package use-package))

(use-package which-key
  :diminish
  :config
  ;; TODO: Replace this with something advice based.
  (defun which-key--compute-binding (binding)
    (copy-sequence (if-let ((docstring (get binding 'variable-documentation)))
                       (format "+%s" docstring)
                     (symbol-name
                      (or (and which-key-compute-remaps
                               (command-remapping binding))
                          binding)))))
  (which-key-mode 1))

;; Get org on the table early
(straight-use-package 'org)

(defmacro use-feature (pkg &rest body)
  "`use-package' for stuff that comes with emacs."
  (declare (indent defun))
  `(use-package ,pkg
     :straight (,pkg :type built-in)
     ,@body))

(defconst use-feature-font-lock-keywords
  '(("(\\(use-feature\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\))?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode use-feature-font-lock-keywords)

(use-package no-littering
  :config
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(no-littering-expand-var-file-name "auto-save/\\2") t)
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (setq server-socket-dir (no-littering-expand-var-file-name "server/")))


;; Quitting emacs
(setq confirm-kill-processes nil)

;;;
;;; Support functions & macros

(defmacro for-terminal (&rest body)
  (declare (indent defun))
  (unless (display-graphic-p) `(progn ,@body)))

(defmacro for-gui (&rest body)
  (declare (indent defun))
  (when (display-graphic-p) `(progn ,@body)))

(defmacro for-mac (&rest body)
  (declare (indent defun))
  (when (eq "darwin" system-type) `(progn ,@body)))

;; Leader keys and such.

(defvar pdc-leader "M-m")
(defvar pdc-mode-leader "C-,")

;;
;; Appearance

(use-package doom-themes
  :config
  (load-theme 'doom-zenburn t)
  )

(when (file-exists-p pdcmacs-config-file)
  (load pdcmacs-config-file))

(use-package display-line-numbers
  :hook
  ((conf-mode prog-mode text-mode) . 'display-line-numbers-mode)
  :init
  (setopt
   display-line-numbers-grow-only t
   display-line-numbers-type t
   display-line-numbers-width nil))

;; Custom file stuff

(when (file-exists-p custom-file)
  (load custom-file))

(setq gc-cons-threshold (* 2 1000 1000))
