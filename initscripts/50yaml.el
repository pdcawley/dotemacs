;;; 50yaml.el --- YAML mode configuration

(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(define-key yaml-mode-map "\C-m" 'newline-and-indent)

(add-hook 'yaml-mode-hook
 '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
