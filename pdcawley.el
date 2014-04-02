(when (memq window-system '(mac-ns ns))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))

(require 'pdc-utils)
(setq temporary-file-directory (expand-file-name "~/tmp/emacstmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory t))

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(defun pdc/turn-on-abbrev-mode ()
  "A boring hook to turn abbrev mode on"
  (abbrev-mode 1))

(require 'pdc-support)

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (or (looking-at "['\"]") (eolp)) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(setenv "PATH" (shell-command-to-string "echo $PATH"))

(defun pdc/enable-commands (cmds)
  (dolist (cmd cmds)
    (put cmd 'disabled nil)))

(defun show-messages ()
  "Show the messages buffer"
  (interactive)
  (switch-to-buffer "*Messages*"))

(bind-key "C-h M" 'show-messages)


(pdc/enable-commands
 '(downcase-region erase-buffer eval-expression
                   narrow-to-page narrow-to-region set-goal-column upcase-region))

(when window-system
  (set-face-attribute
   'default nil
   :height 120
   ;; (let ((width (display-pixel-width)))
   ;;           (cond ((= width 2560) 160)
   ;;                 (t 140)))
   :family "Menlo"))

;;(load-theme 'nzenburn)
;; (require 'zenburn)
;;(color-theme-zenburn)
