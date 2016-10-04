(req-package bookmark+)
(req-package list-register)
(req-package bookmark
  :require (list-register bookmark+ ivy)
  :init
  (setq bmkp-bookmark-map-prefix-keys (concat leader-key " p"))
  (setq bookmark-save-flag 1)
  :general
  (:keymap 'bookmark-map
   "c M" 'my-bookmark-set)
  :config
  (defun dss/bookmark-jump (bookmark)
    (interactive
     (progn
       (bookmark-maybe-load-default-file)
       (list (ivy-completing-read "Jump to bookmark: "
                                  (mapcar 'car bookmark-alist)))))
    (bookmark-jump bookmark))
  (defun my-bookmark-set ()
    (interactive)
    (cl-flet ((bmkp-completing-read-lax
               (prompt &optional default alist pred hist)
               (ivy-completing-read prompt alist pred ni nil hist default)))
      (call-interactively #'bookmark-set)))
  (define-key global-map [remap bookmark-jump] 'dss/bookmark-jump))
