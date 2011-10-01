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

;; Make sure we have font-lock to start with
(require 'font-lock)
(require 'zenburn)
(color-theme-zenburn)
(global-hl-line-mode t)

;; Just say no to splash screens
(setq inhibit-startup-message t)

;; Abbrevs
;(quietly-read-abbrev-file)

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
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/yasnippet/snippets"))
(setq yas/window-system-popup-function
      'yas/x-popup-menu-for-template)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'align)

(setq pdc:elisp-external-dir
      (expand-file-name "elisp/external" dotfiles-dir))

(dolist (project (directory-files pdc:elisp-external-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))
                 

;; end 00setup.el
