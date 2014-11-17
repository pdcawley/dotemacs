(setq dss-yas-snippets-dir (concat dss-dotfiles-dir "dss-yas-snippets"))

(require 'yasnippet)
(setq yas/trigger-key (kbd "C-c <tab>"))
(setq yas/wrap-around-region t)

(yas/initialize)
;; (yas/load-directory (concat dss-vendor-dir "yasnippet/snippets"))
(yas/load-directory dss-yas-snippets-dir)

(provide 'dss-yas)
