;;; 00setup.el --- Personal initial setup code
;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains miscellaneous startup code which needs to / can
;; run early-on

;; TODO: make this generic

;; Make sure we have font-lock to start withco
(require 'font-lock)
(require 'paren)
(require 'cc-vars)

(fset 'yes-or-no-p 'y-or-n-p) ; less typing

(setq warning-suppress-types nil)       ; get rid of strange warning that happens in 23.2
;;; http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/ca5bdf88e61c0a94

;; visual frame / modeline settings
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-title-format "Emacs--> %S: %f")


;;;  tabs and column defs
(setq-default indent-tabs-mode nil) ; no fucking tabs!
(setq tab-width 4) ; but just in case
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92)))
(setq fill-column 80)
(setq c-basic-offset 4)

;;; scrolling behaviour
(setq scroll-step 1)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position nil)

;;; miscellaneous minor/helper modes
(setq kill-whole-line t)

;; Remember window combinations
(winner-mode)

;; Abbrevs
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; I like UTF-8
(prefer-coding-system 'utf-8)

;; Disable advanced featres? Bah.
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Some minor modes I usually enjoy
(column-number-mode t)
(global-font-lock-mode 1)
(delete-selection-mode 1)
(setq kill-whole-line t)

(setq show-paren-style 'parenthesis)
(show-paren-mode t)


;; Load up a bunch of common libraries
(req-package ffap)
(req-package url)
(req-package uniquify)
;(req-package ansi-color)
(req-package recentf)



;; RELAX-NG editing
(req-package rnc-mode
  :ensure t
  :mode ("\\.rnc\\'" . rnc-mode))

(eval-when-compile (require 'cl))
;; (setq yas/window-system-popup-function
;;       'yas/x-popup-menu-for-template)

(require 'align)

;; Utility functions etc...

(defun region-or-thing (thing)
  "Return a vector containing the region and its bounds if there is one
or the thing at the point and its bounds if there is no region"
  (if (use-region-p)
      (vector (buffer-substring-no-properties (region-beginning) (region-end))
              (region-beginning) (region-end))
    (let* ((bounds (bounds-of-thing-at-point thing))
           (beg (car bounds))
           (end (cdr bounds)))
      (vector (buffer-substring-no-properties beg end) beg end))))

(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(eval-when-compile (require 'dss-tmp-files))
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(let ((pdc/bdir (concat temporary-file-directory "/backups"))
      (pdc/adir (concat temporary-file-directory "/autosaves")))
  (setq backup-directory-alist `((".*" . ,pdc/bdir)))
  (setq auto-save-file-name-transforms `((".*" ,pdc/adir t)))
  (setq auto-save-list-file-prefix emacs-tmp-dir)
  (unless (file-exists-p pdc/bdir) (make-directory pdc/bdir))
  (unless (file-exists-p pdc/adir) (make-directory pdc/adir)))

(defgroup pdc nil
  "A group of personal customizations"
  :group 'emacs)

(set-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

