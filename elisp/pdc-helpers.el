;;; -*- lexical-binding: t -*-

(eval-when-compile 
  (use-package s)
  (use-package ivy)
  (use-package align))


;; Steve Yegge tips
(defun pdc|rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t   (rename-file name new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil)))))))

;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun pdc|move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (directory (if (string-match dir "\\(?:/\\|\\\\)$")
                        (substring dir 0 -1)
                      dir))
         (newname (concat directory "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

(defun pdc/dasherize (string)
  (s-replace "_" "-" string))

(defun pdc|align-last-sexp (&optional use-align-regexp)
  (interactive "P")
  (require 'align)
  (save-excursion
    (backward-sexp)
    (let ((beg (point)))
      (forward-sexp)
      (if (not use-align-regexp)
          (align beg (point))
        (align-regexp
         beg
         (point)
         (concat "\\(\\s-*\\)"
                 (read-string "Align regexp: " "" 'align-regexp-history))
         1 align-default-spacing nil)))))

(defun pdc|align-has ()
  (interactive)
  (save-excursion
    (re-search-backward "has ")
    (re-search-forward "(")
    (backward-char)
    (forward-sexp)
    (pdc|align-last-sexp)))

(defun pdc|align (&optional use-align-regexp)
  (interactive "P")
  (save-excursion
    (cond ((region-active-p) (align (mark) (point)))
          (t
           (ignore-errors (up-list))
           (pdc|align-last-sexp use-align-regexp)))))

(defun pdc|string-quote-last-sexp (&optional quote)
  (interactive)
  (let ((quote (or quote "'")))
    (insert quote)
    (save-excursion
      (backward-char)
      (backward-sexp)
      (insert quote))))

(defun pdc|quote-behind (&optional quote)
  (interactive)
  (skip-chars-backward ",:[[:blank:]]")
  (let ((quote (or quote "'")))
    (cond ((looking-back quote 1)
           (save-excursion
             (backward-sexp)
             (delete-char 1)
             (backward-sexp)
             (insert quote)))
          (t
           (pdc|string-quote-last-sexp quote)))))

(defun pdc|quote-from-last-comma (&optional quote)
  (interactive)
  (let ((quote (or quote "'")))
    (skip-chars-forward ",;[:blank:]")
    (insert quote)
    (save-excursion
      (search-backward ",")
      (skip-chars-forward ",[:blank:]")
      (insert quote))))

(defun pdc|doublequote-behind ()
  (interactive)
  (pdc|quote-behind "\""))

(defun pdc/use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end)
              (region-beginning)))))

(defun pdc/default-for-read (&optional thing-type)
  (unless (pdc/use-region-p)
    (thing-at-point (or thing-type 'symbol))))

(defun isearch-yank-thing-at-point (thing-type)
  (let ((thing (thing-at-point thing-type)))
    (isearch-yank-string
     (with-temp-buffer
       (insert thing)
       (buffer-substring-no-properties (point-min)
                                       (point-max))))))


(defun isearch-yank-word-at-point ()
  "Yank the word at the current point"
  (interactive)
  (isearch-yank-thing-at-point 'word))


(defun isearch-yank-symbol-at-point ()
  "Yank the symbol at the current point"
  (interactive)
  (isearch-yank-thing-at-point 'symbol))

(defun pdc|eval-and-replace ()
  "Repleace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (pp (eval (read (current-kill 0)))
          (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun pdc|smarter-move-beginning-of-line (arg)
  "Move point back to indention or beginning of line.

Move point ot the first non-whitespace character of this line. If
point is already there, move to the beginning of the line. Effectively
toggle between the first non-whitespace character and the beginning of
the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If point
reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (let ((arg (or arg 1)))
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((old-point (point)))
      (back-to-indentation)
      (when (= (point) old-point)
        (move-beginning-of-line 1)))))

(defun pdc|start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.

Otherwise switch to the buffer anmed bUFFER-NAME. Don't clobber the
current buffer."
  (if (get-buffer buffer-name)
      (switch-to-buffer-other-window buffer-name)
    (split-window-sensibly (selected-window))
    (other-window 1)
    (funcall function)))

(defun pdc|visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (pdc|start-or-switch-to (lambda ()
                        'ansi-term (getenv "SHELL"))
                      "*ansi-term*"))

(defun pdc|visit-ielm ()
  "Switch to default `ielm' buffer.

Start `ielm' if it's not already running"
  (interactive)
  (pdc|start-or-switch-to 'ielm "*ielm*"))

(provide 'pdc-helpers)
