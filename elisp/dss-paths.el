(setq dss-dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))
;; dss-dotfiles-dir added to load-path by init.el
(setq dss-vendor-dir (concat dss-dotfiles-dir "vendor/"))
(add-to-list 'load-path dss-vendor-dir)
(setq dss-ephemeral-dir "~/.emacs.ephemeral/")
(unless (file-exists-p dss-ephemeral-dir)
  (mkdir dss-ephemeral-dir))
(provide 'dss-paths)
