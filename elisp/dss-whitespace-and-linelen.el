;(load-library "col-highlight") ; for highlighting the current col

(setq require-final-newline t) ; auto-insert final newlines in all files

(require 'lineker)
(defun dss/load-lineker-mode ()
  (interactive)
  (setq lineker-column-limit 90)
  (lineker-mode))

;; whitespace highlighting and cleanup

(require 'show-wspace)
(defun dss/show-ws ()
  (interactive)
  (setq show-trailing-whitespace t)
  (ws-highlight-tabs)
  ;; (hl-line-mode 1)
  )

(defun dss/del-last-space (&optional replacement)
  (interactive)
  (let ((replacement (or replacement "")))
    (save-excursion
      (search-backward-regexp "\\([^ ]\\)\\( +\\)")
      (replace-match replacement nil nil nil 2))))

(defun dss/del-last-space-2 ()
  (interactive)
  (save-excursion
    (if (string-equal (char-to-string (char-before)) " ")
        (progn (backward-word) (dss/del-last-space))
      (dss/del-last-space))
    )
  (if (not (string-equal (char-to-string (char-before)) " "))
      (insert " ")))

(defun dss/less-space ()
  (interactive)
  (dss/del-last-space " "))

(defun dss/whitespace-cleanup ()
  "Trim all trailing whitespace in the current buffer, and untabify."
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (untabify (point-min) (point-max))))

(defun dss/install-whitespace-cleanup-hook ()
  "Add this to any major mode hook to show whitespace during editing
   and trip it before saving"
  (dss/show-ws)
  (add-hook 'before-save-hook 'dss/whitespace-cleanup nil t))

(add-hook 'emacs-lisp-mode-hook 'dss/install-whitespace-cleanup-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-whitespace-and-linelen)
