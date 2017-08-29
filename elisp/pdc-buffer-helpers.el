;;; pdc-buffer-helpers.el --- Buffer helps -*- lexical-binding: t; -*-

;;; Spacemacs lifts
(defun pdc/move-buffer-to-window (windownum &optional follow-focus-p)
  "Moves a buffer to window WINDOWNUM, using the window numbering.

FOLLOW-FOCUS-P controls whether the focus moves with the buffer."
  (interactive "NWindow: ")
  (let* ((follow-focus-p (or follow-focus-p (< windownum 0)))
         (windownum (abs windownum)))
    (let* ((b (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum)))
      (unless (eq w1 w2)
        (set-window-buffer w2 b)
        (switch-to-prev-buffer)
        (unrecord-window-buffer w1 b)))
    (when follow-focus-p (select-window (winum-get-window-by-number windownum)))))


(defun pdc/swap-buffers-to-window (windownum &optional follow-focus-p)
  "Swaps visible bufers between active window and window WINDOWNUM.

FOLLOW-FOCUS-P controls whether the focus moves with the buffer"
  (interactive "NWindow: ")
  (let* ((follow-focus-p (or follow-focus-p (< windownum 0)))
         (windownum (abs windownum)))
    (let* ((b1 (current-buffer))
           (w1 (selected-window))
           (w2 (winum-get-window-by-number windownum))
           (b2 (window-buffer w2)))
      (unless (eq w1 w2)
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (unrecord-window-buffer w1 b1)
        (unrecord-window-buffer w2 b2)))
    (when follow-focus-p (winum-select-window-by-number windownum))))

(dotimes (i 9)
  (let ((n (1+ i)))
    (eval `(defun ,(intern (format "buffer-to-window-%s" n)) (&optional arg)
            ,(format "Move buffer to the window with number %i." n)
            (interactive "P")
            (if arg
                (pdc/swap-buffers-to-window ,n t)
              (pdc/move-buffer-to-window ,n t))))
    (eval `(defun ,(intern (format "move-buffer-to-window-no-follow-%s" n)) ()
       (interactive)
       (pdc/move-buffer-to-window ,n nil)))
    (eval `(defun ,(intern (format "swap-buffer-window-no-follow-%s" n)) ()
       (interactive)
       (pdc/swap-buffers-to-window ,n nil)))))

(defun pdc/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate
         (frame-parameter (window-frame window) 'buffer-predicate)))
    (switch-to-buffer
     (or (cl-find-if (lambda (buffer)
                       (and (not (eq buffer current-buffer))
                            (or (null buffer-predicate)
                                (funcall buffer-predicate buffer))))
                     (mapcar #'car (window-prev-buffers window)))
         (other-buffer current-buffer t)))))

(defun pdc/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then also kill the window."
  (interactive "P")
  (cond ((window-minibuffer-p)
         (abort-recursive-edit))
        ((equal '(4) arg)
         (kill-buffer-and-window))
        (t (kill-buffer))))

(defun pdc/safe-erase-buffer ()
  "Prompt before erasing the content of the file."
  (interactive)
  (when (y-or-n-p (format "Erase content of buffer %s ? " (current-buffer)))
    (erase-buffer)))

;; from https://github.com/gempesaw/dotemacs/blob/emacs/dg-defun.el
(defun pdc/kill-matching-buffers-rudely (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP. This
function, unlike the built-in `kill-matching-buffers` does so
WITHOUT ASKING. The optional second argument indicates whether to
kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

;; found at http://emacswiki.org/emacs/KillingBuffers
(defun pdc/kill-other-buffers (&optional arg)
  "Kill all other buffers.
If the universal prefix argument is used then will the windows too."
  (interactive "P")
  (when (y-or-n-p (format "Killing all buffers except \"%s\"? "
                          (buffer-name)))
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (when (equal '(4) arg) (delete-other-windows))
    (message "Buffers deleted!")))

(defun pdc/new-empty-buffer ()
  "Create a new buffer called untitled(<n>)"
  (interactive)
  (let ((newbuf (generate-new-buffer-name "untitled")))
    (switch-to-buffer newbuf)))

;; http://stackoverflow.com/a/10216338/4869
(defun pdc/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun pdc/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

(defun pdc/safe-revert-buffer ()
  "Prompt before reverting the file."
  (interactive)
  (revert-buffer nil nil))

(defun pdc/switch-to-scratch-buffer ()
  "Switch to the `*scratch' buffer. Create it first if needed."
  (interactive)
  (let ((exists (get-buffer ("*scratch*"))))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(provide 'pdc-buffer-helpers)
