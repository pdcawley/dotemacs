(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode 1)))
