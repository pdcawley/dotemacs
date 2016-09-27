(require 'req-package)
(when (memq window-system '(mac-ns ns))
  (req-package exec-path-from-shell
    :config
    (progn
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "GOPATH"))))

(require 'pdc-utils)

(setq server-socket-dir (format (expand-file-name "~/tmp/emacs%d") (user-uid)))

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

(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))

;;(load-theme 'nzenburn)
;; (require 'zenburn)
;;(color-theme-zenburn)
