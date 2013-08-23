;; 02ido.el -- Customization for ido
;; -*- lexical-binding: t -*-

(use-package ido
  :init
  (progn
    (add-hook 'ido-setup-hook 'llasram/ido-extra-keys)

    (defun llasram/ido-extra-keys ()
      "Add my keybindings for ido."
      (define-key ido-completion-map "\C-n" 'ido-next-match)
      (define-key ido-completion-map "\C-f" 'ido-next-match)
      (define-key ido-completion-map "\C-p" 'ido-prev-match)
      (define-key ido-completion-map "\C-b" 'ido-prev-match)
      (define-key ido-completion-map " "    'ido-exit-minibuffer)))
