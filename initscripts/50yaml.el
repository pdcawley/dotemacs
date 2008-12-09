;;; 50yaml.el --- YAML mode configuration

(require 'yaml-mode)

(add-hook 'yaml-mode-hook
 '(lambda ()
    (define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)))
