(eval-when-compile (require 'req-package))

(req-package multiple-cursors
  :bind
  (("C-S-c" . nil)
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-e" . mc/edit-ends-of-lines)
   ("C-S-c C-a" . mc/edit-beginnings-of-lines)
   ("C-. m e" . mc/edit-lines)
   ("C-. m >" . mc/mark-next-like-this)
   ("C-. m <" . mc/mark-previous-line-like-this)
   ("C-. m a" . mc/mark-all-like-this)))

(provide 'init-cursor)
