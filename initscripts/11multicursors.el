(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)

(require 'multiple-cursors)

;; (defun pdc/edit-marks-or-lines ()
;;   (interactive)
;;   (if mm/master
;;       (mc/switch-from-mark-multiple-to-cursors)
;;     (mc/edit-lines)))

;;  (defun mc/enable-function (func)
;;   "Marks a function as enabled during multi cursor operations"
;;   (interactive
;;    (list (read-command "Command to enable for mc ops: " last-command)))
;;   (put func 'mc--enabled t))

;; (defun mc/disable-function (func)
;;   "Marks a function as enabled during multi cursor operations"
;;   (interactive
;;    (list (read-command "Command to enable for mc ops: " last-command)))
;;   (put func 'mc--enabled t))

;; (put 'forward-sexp 'mc--enabled t)
;; (put 'cperl-electric-lbrace 'mc--enabled t)
;; (put 'cperl-electric-brace 'mc--enabled t)
;; (put 'cperl-electric-semi 'mc--enabled t)

;; (mapc #'mc/mark-supported-cmd
;;       '(forward-sexp backward-sexp cperl-electric-semi cperl-electric-brace
;;                      cperl-electric-lbrace))

;;customize-mark-to-save

(defadvice mc/mark-supported-cmd (after pdc/remember-mc-supported-cmds activate)
  "Remember that we support this command next run"
  (when pdc/mc-cmds
    (customize-save-variable 'pdc/mc-cmds (adjoin (ad-get-arg 0) pdc/mc-cmds))))

(defun pdc/reset-mc-cmds (option cmds)
  "Mark the list of commands as enabled for mc"
  (unwind-protect
      (progn
        (ad-deactivate 'mc/mark-supported-cmd)
        (mapc #'mc/mark-supported-cmd cmds)
        (set-default option cmds))
    (ad-activate 'mc/mark-supported-cmd )))

(defcustom pdc/mc-cmds
  '(forward-sexp backward-sexp cperl-electric-semi cperl-electric-brace
                 cperl-electric-lbrace)
  "A list of commands that we're happy to use with multi cursors"
  :type
  '(repeat (restricted-sexp :match-alternatives (commandp)))
  :set 'pdc/reset-mc-cmds
  :initialize 'custom-initialize-reset
  )

