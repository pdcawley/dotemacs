;;; 60eshell.el --- Configuration and extension for eshell

;; eshell foo
(defun eshell-edit (&rest files)
  "Invoke `find-file' on all files."
  (if (listp (car files))
      (progn
        (let ((files2 (car files)))
          (while files2
            (find-file (pop files2)))))
    (while files
      (find-file (pop files)))))

(defalias 'eshell/emacs 'eshell-edit)
(defalias 'eshell/vi 'eshell-edit)

(add-hook 'eshell-mode-hook
  '(lambda ()
     (define-key eshell-mode-map "\C-a" 'eshell-bol)))
