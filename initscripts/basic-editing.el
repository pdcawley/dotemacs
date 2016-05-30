(req-package flymake
  :config
  (setq flymake-log-level 0))


(setq visible-bell nil)
(defun dss/set-undo-boundary ()
  (interactive)
  (undo-boundary))

(defun dss/cap-sentence ()
  (interactive)
  (save-excursion
    (backward-sentence)
    (capitalize-word 1)))

(defun dss/quote-region (start end &optional c)
  (interactive "r")
  (let ((c (or c "\"")))
    (save-excursion
      (goto-char start)
      (insert c)
      (goto-char (+ 1 end))
      (insert c))))

(defun dss/single-quote-region (start end)
  (interactive "r")
  (dss/quote-region start end "'"))

(defun dss/yank-and-indent ()
  (interactive)
  (yank)
  (call-interactively 'indent-region))


(require 'misc)      ; forward-to-word & backward-to-word

(req-package visible-mark
  :config
  (global-visible-mark-mode t))

(req-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(defun dss/clone-line ()
  (interactive)
  (beginning-of-line)
  (k2-copy-whole-line)
  (open-next-line)
  (forward-line)
  (yank)
  (back-to-indentation))

(defun dss/goto-line (line)
  "A simplified, single buffer version of the standard command
  that work even if the buffer is narrowed"
  (interactive "nLine:")
  ;; Leave mark at previous position
  (or (region-active-p) (push-mark))
  ;; Move to the specified line number in that buffer.
  (save-restriction
    (goto-char (point-min))
    (if (eq selective-display t)
        (re-search-forward "[\n\C-m]" nil 'end (1- line))
      (forward-line (1- line)))))

(req-package goto-last-change
  :config
  (setq highlight-changes-visibility-initial-state nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://github.com/al3x/emacs/blob/master/utilities/slick-copy.el
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dss/apply-to-region (func)
  (when (region-active-p)
    (insert (funcall func (delete-and-extract-region
                           (region-beginning)
                           (region-end))))))
