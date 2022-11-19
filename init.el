;; init.el --- -*- lexical-binding: t; -*-

;;;
;;; Bootstrap some packages

(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;;; user-emacs-directory structure

(defvar pdcmacs-config-file (expand-file-name "config.el"  user-emacs-directory)
  "Our post-init config file.")

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

;;; Add some C-x 8 helpers for things I type commonly
;;; (so far that's just '…')

(define-key 'iso-transl-ctl-x-8-map ";" "…")
(define-key 'iso-transl-ctl-x-8-map "\\" "λ")

;;; Tweak custom-file's location
(setq custom-file (expand-file-name "preferences.el" pdcmacs-etc-directory))


;; Quiet a warning
(customize-set-variable 'large-file-warning-threshold (* 100 1000 1000))

(setq straight-use-package-by-default t)
(straight-use-package 'which-key)
(which-key-mode 1)
(straight-use-package 'general)
(require 'general-autoloads)
(straight-use-package 'use-package)
(require 'use-package)
(use-package diminish)

(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(expand-file-name "auto-save/\\2" pdcmacs-var-directory) t)
        (".*" ,(expand-file-name "auto-save/" pdcmacs-var-directory) t)))

(setq server-socket-dir (expand-file-name "server/" pdcmacs-var-directory))


;; Quitting emacs
(setq confirm-kill-processes nil)

;;;
;;; Support functions & macros

(defmacro for-terminal (&rest body)
  (declare (indent defun))
  (unless window-system `(progn ,@body)))

(defmacro for-gui (&rest body)
  (declare (indent defun))
  (when window-system `(progn ,@body)))

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
  ;; (load-theme 'doom-zenburn t)
  )

(when (file-exists-p pdcmacs-config-file)
  (load pdcmacs-config-file nil 'nomessage))

(use-package display-line-numbers
  :hook
  ((conf-mode prog-mode text-mode) . 'display-line-numbers-mode)
  :init
  (setq-default
   display-line-numbers-grow-only t
   display-line-numbers-type t
   display-line-numbers-width 2))

;; Custom file stuff

(load custom-file 'noerror 'nomessage)

(setq gc-cons-threshold (* 2 1000 1000))
