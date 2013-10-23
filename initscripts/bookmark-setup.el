(use-package bookmark
  :disabled t
  :defer t
  :config
  (progn
    (use-package bookmark+)

    (defun my-bookmark-set ()
      (interactive)
      (flet ((bmkp-completing-read-lax
              (prompt &optional default alist pred hist)
              (completing-read prompt alist pred ni nil hist default)))
        (call-interactively #'bookmark-set)))

    (bind-key "C-x r m" 'my-bookmark-set)))
