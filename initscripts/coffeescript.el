(req-package coffee-mode
  :disabled (not window-system)
  :bind
  (:map coffee-mode-map
    ("C-x C-e" . slime-js-coffee-eval-current)
    ("C-c v" . slime-js-coffee-eval-buffer)))
