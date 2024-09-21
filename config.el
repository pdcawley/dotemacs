;;; -*- lexical-binding: t; -*-

(defgroup pdcmacs nil
  "Pdcmacs customization.")

(use-package xref)

(require 'rx)

(use-package ace-window)

(use-package dash
  :config
  (dash-enable-font-lock))
(use-package s)
(use-package f)
(use-package kv)
(use-package ht)

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f)
  (require 'kv)
  (require 'ht))

;; Speedup with auto-compile


;;;
;;; Text wrangling

;;; Repeat mode stuffs

(use-package repeat
  :disabled
  :custom
  (repeat-echo-function #'ignore)
  :config
  (repeat-mode t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yaml)



(use-package powerline
  :hook
  (after-init . powerline-default-theme))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Movement/jumping
(defvar pdc-jump-map (make-sparse-keymap))

(use-package avy
  :bind
  (("M-m j j" . avy-goto-char-timer)
   ("M-m j b" . avy-goto-char)
   ("M-m j '" . avy-goto-char-2)
   ("M-m j w" . avy-goto-word-1)))

(use-package imenu
  :bind
  (("M-m j i" . imenu))
  :hook
  (font-lock-mode .  pdc/try-to-add-imenu)
  :custom
  (imenu-sort-function 'imenu--sort-by-name)
  :init
  (defun pdc/try-to-add-imenu ()
    "Add Imenu to modes that have font-lock-mode activated."
    (condition-case nil (imenu-add-to-menubar "Imenu")
      (error nil))))

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'left)
  (imenu-list-size 40))

(use-package multiple-cursors
  :bind
  (:prefix "M-m m"
           :prefix-map pdc-multi-map
           :prefix-docstring "multi"
           ("a" . mc/edit-beginnings-of-lines)
           ("e" . mc/edit-ends-of-lines)
           ("^" . mc/edit-beginnings-of-lines)
           ("$" . mc/edit-ends-of-lines)
           ("m" . mc/edit-lines))
  :config)

(use-feature outline)

(use-feature ediff :after outline
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-prepare-buffer . show-all)
  (ediff-quit           . winner-undo))

;; (use-package flycheck
;;   :commands global-flycheck-mode
;;   :diminish " ⓢ"
;;   :hook
;;   (after-init . global-flycheck-mode))

;;; Setup common lisp mode stuff

(use-package calendar
  :custom
  (calendar-date-style 'iso))

(use-package editorconfig
  :diminish
  :hook
  (after-init . editorconfig-mode))

(use-package pdf-tools :if (display-graphic-p))

(use-feature notifications)

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  (keymap-global-set "M-m d E" '("envrc" . envrc-command-map)))

(use-package json-ts-mode
  :mode ("\\.noisette\\'" "\\.json\\'"))

(use-package project :after envrc
  :bind (:map project-prefix-map
              ("C" . 'recompile)
              ("s" . 'consult-ripgrep))
  :config (dolist (func '(project-find-file project-find-dir project-eshell))
            (advice-add func :after #'envrc-allow)))

(use-package geiser-guile)

(use-package suggest
  :commands (suggest))

(use-package casual-info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))
(setq font-lock-mode-hook (cdr font-lock-mode-hook))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package haskell-mode)

(require 'pdcmacs-feeds)
(require 'pdcmacs-org)
(require 'pdcmacs-hugo-support)
(require 'pdcmacs-webservice)
