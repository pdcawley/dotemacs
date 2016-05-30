(req-package deft
  :require org
  :bind
  ("<f10>" . deft)
  :config
  (setq deft-extension "org"
        deft-directory "~/Dropbox/notes"
        deft-text-mode 'org-mode))
