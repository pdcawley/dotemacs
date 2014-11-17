(require 'dss-paths)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bookmarks & registers
(setq bookmarkp-bmenu-state-file (concat dss-ephemeral-dir "bmk-bmenu-state.el"))
;(require 'bookmark+)
(defun dss/bookmark-jump (bookmark)
  (interactive
   (progn
     (require 'bookmark)
     (bookmark-maybe-load-default-file)
     (list (ido-completing-read "Jump to bookmark: "
                                (mapcar 'car bookmark-alist)))))
  (bookmark-jump bookmark))

; study http://emacs-fu.blogspot.com/2009/01/using-registers.html
(require 'list-register)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://www.nongnu.org/bm/
;;; http://emacsblog.org/2007/03/22/bookmark-mania/
(require 'bm)
(setq bm-repository-file (concat dss-ephemeral-dir "bm-repository"))
(setq bm-restore-repository-on-load t)
(defun dss/bm-after-save-hook ()
  (bm-buffer-save-all)
  ;; (bm-repository-save)
  )
(add-hook 'after-save-hook 'dss/bm-after-save-hook)

;; (global-set-key (kbd "<M-f2>") 'bm-toggle)
;; (global-set-key (kbd "<f2>")   'bm-next)
;; (global-set-key (kbd "<S-f2>") 'bm-previous)

(setq-default bm-buffer-persistence t)
(add-hook' after-init-hook 'bm-repository-load)
(add-hook 'find-file-hooks 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)

(defadvice bm-show-mode
  (around bm-show-mode-with-linum activate)
  ad-do-it
  (linum-mode))

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-bookmarks-registers)
