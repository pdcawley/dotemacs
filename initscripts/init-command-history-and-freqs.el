(require 'dss-paths)

(req-package savehist
  :config
  (setq savehist-file (concat dss-ephemeral-dir "history"))
  (setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  (savehist-mode 1))

(require 'tramp)

(req-package saveplace
  :config
  (setq save-place-file (concat dss-ephemeral-dir "saveplace"))
  (setq-default save-place t))


(req-package recentf
  :init
  (setq recentf-save-file (concat dss-ephemeral-dir "recentf"))
  :config
  (recentf-mode 1)
  (setq recentf-max-saved-items 150)
  (setq recentf-max-menu-items 60)
  (run-with-timer (* 20 60) (* 2 60 60) (lambda () (recentf-save-list)))
  (add-hook 'recentf-dialog-mode-hook
        (lambda ()
          (linum-mode +1))))

