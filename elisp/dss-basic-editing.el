(require 'diminish)
(require 'filladapt)
(require 'flymake)
(setq flymake-log-level 0)

(setq-default filladapt-mode t)
(diminish 'filladapt-mode "")
(diminish 'abbrev-mode "Ab.")

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

(require 'visible-mark)
(global-visible-mark-mode t)

(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode "")

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

(require 'goto-last-change)             ;there is another version of this library called goto-chg.el

;; see
;; http://nicolas-lara.blogspot.com/2009/11/emacs-mark-stack.html
;; http://www.bloomington.in.us/~brutt/marker-visit.el

;;; http://emacs-fu.blogspot.com/2009/05/tracking-changes.html
;; (require 'hilit-chg)
(setq highlight-changes-visibility-initial-state nil)
(global-highlight-changes-mode -1)
;; (global-set-key (kbd "<f6>")      'highlight-changes-visible-mode) ;; changes
;; ;; remove the change-highlight in region
;; (global-set-key (kbd "S-<f6>")    'highlight-changes-remove-highlight)
;; (global-set-key (kbd "<M-prior>") 'highlight-changes-next-change)
;; (global-set-key (kbd "<M-next>")  'highlight-changes-previous-change)


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

(provide 'dss-basic-editing)
