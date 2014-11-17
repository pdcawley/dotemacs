
(defun dss/-get-hook-funcs (hook)
  (delq nil (mapcar
             (lambda (e) (if (symbolp e) e))
             hook)))

(defun dss/-get-hook-funcs-names (hook)
  (mapcar 'symbol-name
          (dss/-get-hook-funcs
           (if (symbolp hook)
               (symbol-value hook)
             hook))))

(defun dss/-get-all-hooks ()
  (let (hlist (list))
    (mapatoms (lambda (a)
                (if (and (not (null (string-match ".*-hook"
                                                  (symbol-name a))))
                         (not (functionp a)))
                    (add-to-list 'hlist a))))
    hlist))

;;; mapatoms (symbolp x) (functionp x) (commandp (symbol-function x)) (fbound x)
;;; also see http://stackoverflow.com/questions/605785/how-do-i-get-a-list-of-emacs-lisp-non-interactive-functions

(defun dss/remove-from-hook (hook fname &optional local)
  (interactive
   (let ((hook (intern (ido-completing-read
                        "Which hook? "
                        (mapcar #'symbol-name (dss/-get-all-hooks))))))
     (list hook
           (ido-completing-read "Which? " (dss/-get-hook-funcs-names hook)))))
  (remove-hook hook
               (if (stringp fname)
                   (intern fname)
                 fname)
               local))

(defun dss/remove-after-save-hook (fname)
  (interactive (list (ido-completing-read
                      "Which? "
                      (dss/-get-hook-funcs-names after-save-hook))))
  (dss/remove-from-hook 'after-save-hook fname t))

(defun dss/add-after-save-hook (fname)
  (interactive "aWhich function: ")
  (add-hook 'after-save-hook fname t t))


(provide 'dss-hook-management)
