(req-package go-mode
  :require flycheck pretty-mode
  :ensure t
  :config
  (progn
    (defun pdc/go-mode-hook ()
      "Hook into go-mode"
      (add-hook 'before-save-hook #'gofmt-before-save))
    (add-hook 'go-mode-hook #'pdc/go-mode-hook)
    (bind-key "M-." 'godef-jump go-mode-map)
    (bind-key "RET" 'newline-and-indent go-mode-map)
    (bind-key "C-j" 'newline go-mode-map)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
    (pretty-add-keywords 'go-mode
                         '(("<-" . ?←)))

    (flycheck-define-checker go-gofmt
      "A Go syntax and style checker using the gofmt utility"
      :command ("gofmt" source-inplace)
      :error-patterns
      ((error line-start (file-name) ":" line ":" column ": " (message)
              line-end))
      :modes 'go-mode)
    (add-to-list 'flycheck-checkers 'go-gofmt)
    (add-hook 'go-mode-hook 'flycheck-mode)))
