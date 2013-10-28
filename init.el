;;(require 'eieio)
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))
(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(load (expand-file-name "load-path" (file-name-directory load-file-name)))

(require 'autoinsert)

;; encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;_ , Utility macros and functions

(require 'pdc-utils)
(setq temporary-file-directory (expand-file-name "~/tmp/emacstmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory t))

(defvar running-alternate-emacs nil)


(if (string-match (concat "/Applications/\\(Misc/\\)?"
                          "Emacs\\([A-Za-z]+\\).app/Contents/MacOS/")
                  invocation-directory)
    (let ((settings (with-temp-buffer
                      (insert-file-contents
                       (expand-file-name "preferences.el" user-emacs-directory))
                      (goto-char (point-min))
                      (read (current-buffer))))
          (suffix (downcase (match-string 2 invocation-directory))))

      (setq running-alternate-emacs t
            user-data-directory
            (replace-regexp-in-string "/data/" (format "/data-%s/" suffix)
                                      user-data-directory))

      (let* ((regexp "/\\.emacs\\.d/data/")
             (replace (format "/.emacs.d/data-%s/" suffix)))
        (dolist (setting settings)
          (let ((value (and (stringp value)
                            (string-match regexp value))
                       (setcar (nthcdr 1 (nth 1 setting)))))
            (if (and (stringp value)
                     (string-match regexp value))
                (setcar (nthcdr 1 (nth 1 setting))
                        (replace-regexp-in-string regexp replace value)))))

        (eval settings)))

  (load (expand-file-name "preferences" user-emacs-directory)))

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(setq server-auth-dir (expand-file-name "~/server/"))
(setq-default gc-cons-threshold 10000000)

(defun pdc/turn-on-abbrev-mode ()
  "A boring hook to turn abbrev mode on"
  (abbrev-mode 1))

(require 'cl)
(require 'pdc-support)
(mapc (lambda (p) (pushnew p exec-path))
      (reverse (list "~/bin" "~/local/node/bin" "~/perl5/perlbrew/bin"
                     "/usr/local/bin"
                     "/usr/local/sbin" "/sbin" "/bin" "/usr/bin" "/usr/sbin")))
;;(load custom-file)

(setq custom-file (expand-file-name "preferences.el" user-emacs-directory))
(require 'package)

(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(package-install 'use-package)
(require 'use-package)

(require 'initscripts)
;;(load custom-file)

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

(require 'zenburn)
(color-theme-zenburn)
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))
