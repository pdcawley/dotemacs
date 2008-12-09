;;; 50yaml.el --- YAML mode configuration

(require 'yaml-mode)

(update-auto-mode-binding '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
 '(lambda ()
    (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)))
