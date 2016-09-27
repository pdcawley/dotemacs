(req-package bookmark+)
(req-package list-register)
(req-package bookmark
  :require list-register
  :bind  (("C-x r b" . dss/bookmark-jump)
          ("C-x r m" . my-bookmark-set))
  :config
  (defun dss/bookmark-jump (bookmark)
    (interactive
     (progn
       (bookmark-maybe-load-default-file)
       (list (ido-completing-read "Jump to bookmark: "
                                  (mapcar 'car bookmark-alist)))))
    (bookmark-jump bookmark))
  (defun my-bookmark-set ()
    (interactive)
    (cl-flet ((bmkp-completing-read-lax
               (prompt &optional default alist pred hist)
               (completing-read prompt alist pred ni nil hist default)))
      (call-interactively #'bookmark-set))))
