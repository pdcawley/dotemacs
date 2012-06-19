(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

(require 'mc-mark-multiple-integration)

(defun pdc/edit-marks-or-lines ()
  (interactive)
  (if mm/master
      (mc/switch-from-mark-multiple-to-cursors)
    (mc/edit-lines)))

(global-set-key (kbd "C-S-c C-S-c") 'pdc/edit-marks-or-lines)

(defun mc/enable-function (func)
  "Marks a function as enabled during multi cursor operations"
  (interactive
   (list (read-command "Command to enable for mc ops: " last-command)))
  (put func 'mc--enabled t))

(defun mc/disable-function (func)
  "Marks a function as enabled during multi cursor operations"
  (interactive
   (list (read-command "Command to enable for mc ops: " last-command)))
  (put func 'mc--enabled t))

(put 'forward-sexp 'mc--enabled t)
