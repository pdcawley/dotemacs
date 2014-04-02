-*- lexical-binding: t; -*-

(require 'dash)
(require 's)

;; Adapted from Let over Lambda

(defun mkstr (&rest args)
  (with-output-to-string (dolist (a args)
                           (princ a))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))


(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (s-prefix? "G!" (upcase (symbol-name s)))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (s-prefix? "O!" (upcase (symbol-name s)))))


(defun o!-symbol-to-g!-symbol (s)
  (symb "g!" (substring-no-properties (symbol-name s) 2)))


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (-flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(substring-no-properties (symbol-name s)
                                      2))))
              syms)
         ,@body))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

