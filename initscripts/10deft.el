(when (require 'deft nil 'noerror)
  (setq deft-extension "org"
        deft-directory "~/Dropbox/notes"
        deft-text-mode 'org-mode)

  (global-set-key (kbd "<f10>")  'deft))
