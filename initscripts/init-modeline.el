(eval-when-compile (require 'req-package))

(req-package which-func)

(req-package spaceline
  :config
  (require 'spaceline-config))

(req-package powerline
  :require which-func spaceline
  :init
  (setq powerline-default-separator (if (display-graphic-p) 'brace 'utf-8)
        powerline-height 24)
  :config
  (defun pdc/customize-powerline-faces ()
    "Alter powerline faces to make them work with more themes"
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face))
  (pdc/customize-powerline-faces)
  (defun pdc//restore-powerline (buffer)
    "Restore the powerline in buffer"
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))
      (powerline-set-selected-window)
      (powerline-reset)))

  ;; (dolist (spec '((minor-modes "tmm")
  ;;                 (major-mode "tmM")
  ;;                 (version-control "tmv")
  ;;                 (new-version "tmV")
  ;;                 (point-position "tmp")
  ;;                 (org-clock "tmc")))
  ;;   (let* ((segment (car spec))
  ;;          (status-var (intern (format "spaceline-%S-p" segment))))
  ;;     (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
  ;;                                  :status ,status-var
  ;;                                  :on (setq ,status-var t)
  ;;                                  :off (setq ,status-var nil)
  ;;                                  :documentation ,(format "Show %s in the mode-line."
  ;;                                                          (replace-regexp-in-string
  ;;                                                           "-" " " (format "%S" segment)))
  ;;                                  :evil-leader ,(cadr spec)))))
  (setq spaceline-org-clock-p nil)

  (defun pdc//prepare-diminish ()
    (when spaceline-minor-modes-p
      (setq spaceline-minor-modes-separator
            (if (display-graphic-p) "" " "))))
  (add-hook 'spaceline-pre-hook 'pdc//prepare-diminish)
  (spaceline-toggle-hud-on)
  (spaceline-toggle-line-column-off)
  (spaceline-emacs-theme))

