(require 'dss-paths)
(setq-default bookmark-default-file (concat dss-ephemeral-dir "emacs.bmk"))
(setq bookmark-save-flag 1)

(setq savehist-file (concat dss-ephemeral-dir "history"))
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode 1)

(require 'saveplace)
(setq save-place-file (concat dss-ephemeral-dir "saveplace"))
(setq-default save-place t)

(setq recentf-save-file (concat dss-ephemeral-dir "recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 150)
(setq recentf-max-menu-items 60)
(run-with-timer (* 20 60) (* 2 60 60) (lambda () (recentf-save-list)))
;(add-hook 'find-file-hook (lambda () (recentf-save-list)))
(add-hook 'recentf-dialog-mode-hook
          (lambda ()
            (linum-mode +1)))

(defun dss/ido-choose-from-recentf ()
  ;;from http://www.xsteve.at/prg/emacs/power-user-tips.html
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (let ((home (expand-file-name (getenv "HOME"))))
    (find-file
     (ido-completing-read "Recentf open: "
                          (mapcar (lambda (path)
                                    (replace-regexp-in-string
                                     (concat home "/") "~/"
                                     path))
                                  recentf-list)
                          nil t))))

(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(setq session-save-file (concat dss-ephemeral-dir "session"))
(setq session-save-file-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-command-history-and-freqs)
