(defun dss/yaml-mode-hook ()
  (interactive)
  (define-key yaml-mode-map "\C-m" 'newline-and-indent))

(add-hook 'yaml-mode-hook 'dss/yaml-mode-hook)

(provide 'dss-yaml)
