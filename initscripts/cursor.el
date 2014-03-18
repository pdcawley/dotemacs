(use-package multiple-cursors :bind
  (("C-. m e" . mc/edit-lines)
   ("C-. m >" . mc/mark-next-like-this)
   ("C-. m <" . mc/mark-previous-line-like-this)
   ("C-. m a" . mc/mark-all-like-this)))
