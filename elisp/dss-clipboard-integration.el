(require 'browse-kill-ring)
(require 'dss-elisp-funcs) ; dss/file-to-string

(setq x-select-enable-clipboard t)

;; Ideas and code stolen from
;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html

(defun dss/xsel-cut-function (text &optional push)
  ;; Insert text to temp-buffer, and "send" content to xsel stdin
  (with-current-buffer "*scratch*"
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--input" "-p" "--clipboard"))))

(defun dss/xsel-paste-function()
  ;; Find out what is current selection by xsel. If it is different
  ;; from the top of the kill-ring (car kill-ring), then return
  ;; it. Else, nil is returned, so whatever is in the top of the
  ;; kill-ring will be used.
  (let ((xsel-output (dss/local-shell-command-to-string "xsel --clipboard --output")))
    (unless (or (string-match-p "^xsel: Can't open display" xsel-output)
                (string= (car kill-ring) xsel-output))
      xsel-output)))

(defun dss/x-display-sync ()
  (interactive)
  (setenv "DISPLAY"
          (or (dss/file-to-string "~/.last-display") ":0")))

(defun dss/osx-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "mac_copy" nil 0 nil)))

(defun dss/osx-paste-function()
  (let ((pb (dss/local-shell-command-to-string "mac_paste")))
    (unless (string= (car kill-ring) pb)
      pb)))

(defun dss/tty-x-clipboard-init ()
  (interactive)
  (unless window-system
    (unless (getenv "DISPLAY")
      (dss/x-display-sync))
    (setq interprogram-cut-function 'dss/xsel-cut-function)
    (setq interprogram-paste-function 'dss/xsel-paste-function)))

(defun dss/tty-x-clipboard-disable ()
  (interactive)
  (setq interprogram-cut-function nil)
  (setq interprogram-paste-function nil))

(defun dss/osx-clipboard-enable ()
  (interactive)
  (setq interprogram-cut-function #'dss/osx-cut-function)
  (setq interprogram-paste-function #'dss/osx-paste-function))

;; http://bbs.archlinux.org/viewtopic.php?id=80226
;; and run the following
;; #autocutsel -fork &
;; #autocutsel -selection PRIMARY -fork &

;; also see http://stackoverflow.com/questions/994563/integrate-readlines-kill-ring-and-the-x11-clipboard
;; which I have configured my .bashrc file



;; http://snarfed.org/emacs_keybindings_in_gnu_screens_copy-scrollback_mode

(provide 'dss-clipboard-integration)
