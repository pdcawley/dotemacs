(require 'dss-paths)

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file.

This function comes from the anonymous code on
http://www.emacswiki.org/emacs/DeskTop"
  (when (not (dss/emacs-process-p ad-return-value))
    (setq ad-return-value nil)))


(defun dss/emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil.

This function comes from the anonymous code on
http://www.emacswiki.org/emacs/DeskTop
"
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defun dss/desktop-reset ()
  (interactive)
  (desktop-remove))

(desktop-save-mode 1)

;;(setq history-length 250)
;;(add-to-list 'desktop-globals-to-save 'file-name-history)
;; (setq desktop-buffers-not-to-save
;;       (concat "\\("
;;               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;               "\\)$"))
;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
;; (add-to-list 'desktop-modes-not-to-save 'Info-mode)

(provide 'dss-desktop)
