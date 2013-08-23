(require 'eieio)

(defclass grammar-position ()
  ((index :initarg :index integer)
   (column :initarg :column integer)
   (row :initarg :row integer)))

(defclass grammar-suggestion ()
  ((match :initarg :match :type string)
   (from :initarg :from :type grammar-position)
   (to :initarg :to :type grammar-position)
   (explanations :initarg :explanation_hash
                 :type list)
   (suggestions :initarg :suggestions :type list)))
