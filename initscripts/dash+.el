(require 'dash)

(defun -cdrs (l)
  "Returns all the cdrs of the given list"
  (if l (cons l (-cdrs (cdr l)))
    '(())))

(defun -intercalate (sep lists)
  (apply 'append (-interpose (list sep) lists)))

(defun -heads (l)
  (-map 'reverse (-cdrs (reverse l))))

(defun -scan-from (fn initial-value list)
  (--map (-reduce-from fn initial-value it)
         (reverse (-heads list))))
