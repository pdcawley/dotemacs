(defun show-compilation ()
  (interactive)
  (let ((compile-buf
         (catch 'found
           (dolist (buf (buffer-list))
             (if (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
    (if compile-buf
        (switch-to-buffer-other-window compile-buf)
      (call-interactively 'compile))))

;(bind-key "M-O" 'show-compilation)

(req-package smart-compile
  :disabled t
  :commands smart-compile
  :bind (("C-c c" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error)))
