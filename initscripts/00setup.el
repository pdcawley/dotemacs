;;; 00setup.el --- Personal initial setup code

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains miscellaneous startup code which needs to / can
;; run early-on

;; TODO: make this generic

(add-to-list 'load-path "~/.emacs.d/ruby")
(add-to-list 'load-path "~/.emacs.d/misc")
(add-to-list 'load-path "~/.emacs.d/Emacs")

;; Make sure we have font-lock to start with
(require 'font-lock)

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

;; Make sure we have TRAMP
(require 'tramp)

;; Load ffap's bindings
(require 'ffap)

;; URL is one useful library
(require 'url)

;; RELAX-NG editing
(require 'rnc-mode)
(add-to-list 'auto-mode-alist '("\\.rnc$" . rnc-mode))

;; SVN
(require 'vc)
(require 'vc-git)

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet/snippets")
(setq yas/window-system-popup-function
      'yas/x-popup-menu-for-template)


(require 'align)

;; end 00setup.el
