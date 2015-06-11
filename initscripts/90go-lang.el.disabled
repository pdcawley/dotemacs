(use-package go-mode
  :ensure t
  :init
  (progn
    (defun pdc/go-mode-hook ()
      "Hook into go-mode"
      (add-hook 'before-save-hook #'gofmt-before-save))
    (add-hook 'go-mode-hook #'pdc/go-mode-hook)
    (bind-key "M-." 'godef-jump go-mode-map)
    (use-package fly-check
      :init
      (progn
        (flycheck-declare-checker go-gofmt)
        "A Go syntax and style checker using the gofmt utility"
        :command '("gofmt" source-inplace)
        :error-patterns
        '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
        :modes 'go-mode)
      (add-to-list 'flycheck-checkers 'go-gofmt)
      (add-hook 'go-mode-hook 'flycheck-mode))))


