(require 'relax)

(defun dss/couch-new-doc (id)
  (interactive "sDocument ID: ")
  (let ((url-request-method "PUT")
        (url-request-data "{}"))
    (url-retrieve (relax-url id) 'relax-visit-new-doc)))

(defun dss/couch-new-design-doc (id)
  (interactive
   (list (read-from-minibuffer "Document ID: " "_design/" )))
  (dss/couch-new-doc id))

;; (defun relax-new-doc (choose-id)
;;   "Create a new document. With prefix arg, prompt for a document ID."
;;   (interactive "P")
;;   (let ((url-request-method (if choose-id "PUT" "POST"))
;;         (url-request-data "{}")
;;         (id (if choose-id (read-from-minibuffer "Document ID: "))))
;;     (url-retrieve (relax-url id) 'relax-visit-new-doc)))

(provide 'dss-couchdb)
