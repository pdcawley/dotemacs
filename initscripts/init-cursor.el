(req-package multiple-cursors
  :ensure t
  :bind
  (("C-. m e" . mc/edit-lines)
   ("C-. m >" . mc/mark-next-like-this)
   ("C-. m <" . mc/mark-previous-line-like-this)
   ("C-. m a" . mc/mark-all-like-this)))
