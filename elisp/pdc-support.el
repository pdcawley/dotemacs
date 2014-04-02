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

;; (require 'cl)
;; (unless (boundp 'cl-flet)
;;   (defalias 'cl-flet 'flet))

(defmacro advise-commands (advice-name commands &rest body)
  "Apply advice named ADVICE_NAME to multiple COMMANDS.

The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command (before ,(intern (concat (symbol-name
  command) "-" advice-name)) activate)
                    ,@body))
               commands)))

(provide 'pdc-support)
