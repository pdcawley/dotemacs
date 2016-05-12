(use-package info
  :bind ("C-h C-i" . info-lookup-symbol)

  :config
  (progn
    (add-to-list 'Info-directory-list (expand-file-name "info/" user-emacs-directory))

    (defadvice Info-exit (after remove-info-window activate)
      "When info mode is quit, remove the window."
      (if (> (length (window-list)) 1)
          (delete-window)))))

(use-package info-look
  :ensure t
  :commands info-lookup-add-help)
