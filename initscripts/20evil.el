(use-package evil
  :ensure t
  :config
  (progn
    (bind-key "C-h" 'evil-backward-char evil-normal-state-map)
    (bind-key "C-h" 'evil-replace-backspace evil-replace-state-map)
    (bind-key "C-h" 'evil-delete-backward-char-and-join evil-insert-state-map)))
