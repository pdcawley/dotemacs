(req-package evil
  :bind
  (:map evil-normal-state-map
   ("C-h" . evil-backward-char)
   :map evil-replace-state-map
   ("C-h" . evil-replace-backspace)
   :map evil-insert-state-map
   ("C-h" . evil-delete-backward-char-and-join)))
