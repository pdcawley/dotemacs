(use-package cperl-mode
  :init
  (progn
    (bind-key "C-x C-e" 'slime-js-coffee-eval-current coffee-mode-map)
    (bind-key "C-c v" 'slime-js-coffee-eval-buffer coffee-mode-map)))