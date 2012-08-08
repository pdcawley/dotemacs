;; Various helper functions required by my initscripts

(defun update-auto-mode-binding (alist-entry)
  "Replace an existing binding in auto-mode-alist or add a new binding"
  (let ((pattern (car alist-entry))
        (mode (cdr alist-entry)))
    (let ((binding (assoc pattern auto-mode-alist)))
      (if binding
          (setcdr binding mode)
        (add-to-list 'auto-mode-alist (cons pattern mode))))))

(defun update-auto-mode-bindings (alist)
  (if (listp alist)
      (mapc #'update-auto-mode-binding alist)
    (update-automode-binding alist)))

(require 'cl)
(unless (boundp 'cl-flet)
  (defalias 'cl-flet 'flet))

(provide 'pdc-support)
