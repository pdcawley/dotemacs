;;; config --- unsorted configuration -*- lexical-binding: t; no-byte-compile: t -*-

;;; Commentary:
;;;
;;; This is where all the miscellaneous crap that hasn't been hoisted to
;;; README.org lives

;;; Code:
(use-package xref)

(require 'rx)

(use-package ace-window)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yaml)


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

(use-feature ediff
  :after outline
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

;; (setq font-lock-mode-hook (cdr font-lock-mode-hook))


(provide 'config)
;;; config.el ends here
