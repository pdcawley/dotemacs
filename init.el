(require 'eieio)
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(setq temporary-file-directory (expand-file-name "~/tmp/emacstmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-directory t))

(defun extend-load-path-respecting-subdirs (&rest dirs)
  "Add DIRs to load-path and follow 'subdirs.el' directives"
  (let ((tail dirs) dir)
    (while tail
      (setq dir (car tail))
      (when (not (member dir load-path))
        (push dir load-path)
        (let ((default-directory dir))
          (load (expand-file-name "subdirs.el") t t t))
        (let ((default-directory dir))
          (load (expand-file-name "leim-list.el") t t t)))
      (setq tail (cdr tail)))))


(extend-load-path-respecting-subdirs "~/lisp" dotfiles-dir)
(extend-load-path-respecting-subdirs (concat dotfiles-dir "yasnippet"))
(setq custom-file (concat dotfiles-dir "preferences.el")
      autoload-file (concat dotfiles-dir "loaddefs.el"))

(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

(setq server-auth-dir (expand-file-name "~/server/"))
(setq-default gc-cons-threshold 10000000)

(defun pdc/turn-on-abbrev-mode ()
  "A boring hook to turn abbrev mode on"
  (abbrev-mode 1))

(require 'cl)
(require 'pdc-support)
(load custom-file)
(require 'package)
(add-to-list 'package-archives
             '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'initscripts)
(load custom-file)

(defun transpose-chars (arg)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (interactive "*P")
  (and (null arg) (or (looking-at "['\"]") (eolp)) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(regen-autoloads)

(setenv "PATH" (shell-command-to-string "echo $PATH"))

(put 'narrow-to-region 'disabled nil)

(set-face-attribute 'default nil
                    :height 140
                    :family "Menlo")
