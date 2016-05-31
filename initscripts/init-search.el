(require 'req-package)

(req-package swiper
  :requires multiple-cursors
  :bind
  (("C-s"       . swiper)
   ("C-x 4 C-f" . pdc-find-file-other-window)
   ("C-x 4 f"   . pdc-find-file-other-window))
  :init
  (progn
    (defun pdc-find-file-other-window (&optional initial-input)
      "Forward to `find-file-other-window'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
      (interactive)
      (ivy-read "Find file: " 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action
                (lambda (x)
                  (with-ivy-window
                    (find-file-other-window
                     (expand-file-name x
                                       ivy--directory))))
                :preselect (when counsel-find-file-at-point
                             (require 'ffap)
                             (let ((f (ffap-guesser)))
                               (when f (expand-file-name f))))
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'counsel-find-file))


    (defun swiper-mc ()
      (interactive)
      (unless (require 'multiple-cursors nil t)
        (error "multiple-cursors isn't installed"))
      (let ((cands (nreverse ivy--old-cands)))
        (unless (string= ivy-text "")
          (ivy-set-action
           (lambda (_)
             (let (cand)
               (while (setq cand (pop cands))
                 (swiper--action cand)
                 (when cands
                   (mc/create-fake-cursor-at-point))))
             (mc/maybe-multiple-cursors-mode)))
          (setq ivy-exit 'done)
          (exit-minibuffer))))))

(req-package counsel
  :diminish counsel-mode
  :init
  (progn
    (setq ivy-re-builders-alist
          '((t . ivy--regex-plus))))
  :config
  (progn
    (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
    (counsel-mode)))

(req-package ack-and-a-half
  :bind ("C-. s" . ack-and-a-half)
  :init
  (setq ack-and-a-half-executable (expand-file-name "~/bin/ack"))
  :config
  (defalias 'ack 'ack-and-a-half))

(req-package visual-regexp
  :require multiple-cursors visual-regexp-steroids
  :bind (("C-c m" . vr/mc-mark)))



(provide 'init-search)
