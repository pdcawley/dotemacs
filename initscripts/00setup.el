;;; 00setup.el --- Personal initial setup code

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains miscellaneous startup code which needs to / can
;; run early-on

;; TODO: make this generic

(add-to-list 'load-path (concat dotfiles-dir "ruby/"))
(add-to-list 'load-path (concat dotfiles-dir "misc/"))
(add-to-list 'load-path (concat dotfiles-dir "Emacs/"))
(add-to-list 'load-path (concat dotfiles-dir "html5-el/"))
(add-to-list 'load-path (concat dotfiles-dir "coffee-mode/"))
(add-to-list 'load-path (concat dotfiles-dir "twittering-mode/"))
(add-to-list 'load-path (concat dotfiles-dir "yasnippet/"))
(add-to-list 'load-path (concat dotfiles-dir "gnus/lisp/"))
(add-to-list 'load-path (concat dotfiles-dir "shime/"))
(add-to-list 'load-path (concat dotfiles-dir "emacs-color-theme-solarized/"))
(add-to-list 'load-path (concat dotfiles-dir "elisp/"))

;; Make sure we have font-lock to start withco
(require 'font-lock)
(require 'zenburn)
(color-theme-zenburn)
(global-hl-line-mode t)

;; Just say no to splash screens
(setq inhibit-startup-message t)

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

;; Load up a bunch of common libraries
(require 'tramp)
(require 'ffap)
(require 'url)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)



;; RELAX-NG editing
(require 'rnc-mode)
(update-auto-mode-binding '("\\.rnc\\'" . rnc-mode))

;; SVN
(require 'vc)
(require 'vc-git)

(add-to-list 'load-path (concat dotfiles-dir "/yasnippet"))
(require 'cl)
(require 'yasnippet)
(yas/global-mode 1)
(setq yas/window-system-popup-function
      'yas/x-popup-menu-for-template)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'align)

(setq pdc:elisp-external-dir
      (expand-file-name "elisp/external" dotfiles-dir))

(dolist (project (directory-files pdc:elisp-external-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

(add-to-list 'load-path (concat pdc:elisp-external-dir "/wanderlust/wl"))
(add-to-list 'load-path (concat pdc:elisp-external-dir "/wanderlust/elmo"))


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

(server-start)

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

(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(let ((pdc/bdir (concat temporary-file-directory "/backups"))
      (pdc/adir (concat temporary-file-directory "/autosaves")))
  (setq backup-directory-alist `((".*" . ,pdc/bdir)))
  (setq auto-save-file-name-transforms `((".*" ,pdc/adir t)))
  (setq auto-save-list-file-prefix emacs-tmp-dir)
  (unless (file-exists-p pdc/bdir) (make-directory pdc/bdir))
  (unless (file-exists-p pdc/adir) (make-directory pdc/adir)))

;; end 00setup.el
