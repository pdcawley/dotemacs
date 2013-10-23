(use-package auto-complete-config
  :diminish auto-complete-mode
  :init
  (progn
    (use-package pos-tip)
    (ac-config-default))
  :config
  (progn
    (ac-set-trigger-key "TAB")
    (setq ac-use-menu-map t)

    (bind-key "C-s-?" 'ac-last-help)
    (unbind-key "C-s" ac-completing-map)))
