;;; 50yaml.el --- YAML mode configuration

(req-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent))))
