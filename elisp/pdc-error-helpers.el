;;; -*- lexical-binding: t; -*-
;;; pdc-error-helpers -- Helpers for the navigating through errors
;;
;; Author: Piers Cawley <pdcawley@Piers-MacBook-Pro-2.local>
;;
;;; Commentary;
;;
;;  Mostly lifted from spacemacs
;;
;;; Code:

(defun pdc/error-delegate ()
  "Decide which error API to delegate to.

Delegates to flycheck if it is enabled and the next-error buffer
is not visible. Otherwise delegates to regular Emacs next-error"
  (if (and (bound-and-true-p flycheck-mode)
           (let ((buf (ignore-errors (next-error-find-buffer))))
             (not (and buf (get-buffer-window buf)))))
      'flycheck
    'emacs))

(defun pdc/next-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (pdc/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-next-error)
      (eq 'emacs sys) (call-interactively 'next-error)))))

(defun pdc/previous-error (&optional n reset)
  "Dispatch to flycheck or standard emacs error."
  (interactive "P")
  (let ((sys (pdc/error-delegate)))
    (cond
     ((eq 'flycheck sys) (call-interactively 'flycheck-previous-error)
      (eq 'emacs sys) (call-interactively 'previous-error)))))


(provide 'pdc-error-helpers)

;;; pdc-error-helpers ends here
