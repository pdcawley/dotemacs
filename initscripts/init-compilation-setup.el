;;; -*- lexical-binding: t; -*-

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

(defun gdk (&rest args)
  (apply general-define-key args))


(req-package smart-compile
  :require (dash hydra init-leaders)
  :demand t
  :general
  (pdc|with-leader
   "." '(:ignore t :which-key "compile")
   ". c" 'smart-compile))

(with-eval-after-load 'hydra
  (pdc|general-bind-hydra errornav "."
    ("n" next-error "next")
    ("p" previous-error "prev")
    ("o" occur-next-error "occur next")
    ("d" compilation-display-error "display error")
    ("^" first-error "first" :color blue)))

(provide 'init-compilation-setup)
