(req-package org-journal)

(req-package org
  :requires org-journal
  :diminish org-indent
  :bind
  (("C-c o j" . org-journal-new-entry)
   ("C-c o c" . org-capture)))
