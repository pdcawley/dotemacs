;; this is installed with el-get
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(require 'org-install)
(require 'remember)
(org-remember-insinuate)
(add-hook 'org-mode-hook (lambda () (linum-mode -1)))
(add-hook 'org-agenda-mode-hook (lambda () (linum-mode 1)))
;; much more user specific org-mode stuff is loaded after this in another file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-org-mode)
