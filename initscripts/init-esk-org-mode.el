(req-package org
  :config (progn
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (sh . t)
        (perl . t)))
    (setq org-confirm-babel-evaluate nil)
    (setq org-src-fontify-natively t)
    (setq org-src-tab-acts-natively t)
  ))
