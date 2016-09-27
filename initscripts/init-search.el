(eval-when-compile (require 'cl))
(require 'req-package)


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

(req-package ack-and-a-half
  :bind ("C-. s" . ack-and-a-half)
  :init
  (setq ack-and-a-half-executable "ack")
  :config
  (defalias 'ack 'ack-and-a-half))

(req-package visual-regexp-steroids)
(req-package visual-regexp
  :require multiple-cursors visual-regexp-steroids
  :commands (counsel-find-file counsel-find-file-at-point)
  :bind (("C-c m" . vr/mc-mark)))



(provide 'init-search)
