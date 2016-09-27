;; -*- lexical-binding: t -*-

(req-package nlinum-relative
  :config
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook 'nlinum-relative-on)
  (mapc (lambda (m) (add-hook m 'nlinum-relative-on))
        '(text-mode-hook
          help-mode-hook
          apropos-mode-hook
          diff-mode-hook
          grep-mode-hook
          occur-mode-hook
          conf-mode-hook
          bookmark-bmenu-mode-hook
          c-mode-hook
          sh-mode-hook
          java-mode-hook
          cperl-mode-hook)))

(setq-default visual-line-fringe-indicators '(left-curly-arrow
                                              right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)

;; Fix OSX visual bell
(let ((air-bell-ringing nil))
  (setq ring-bell-function (lambda ()
                             (if (not air-bell-ringing)
                                 (let* ((bg (face-background 'default))
                                        (fg (face-foreground 'default))
                                        (reset (lambda ()
                                                 (set-face-background 'default bg)
                                                 (set-face-foreground 'default fg)
                                                 (setq air-bell-ringing nil))))
                                   (set-face-background 'default
                                                        "NavajoWhite4")
                                   (setq air-bell-ringing t)
                                   (run-with-timer 0.05 nil reset))))))

;; (req-package sunshine
;;   :commands sunshine-forecast
;;   :init
;;   (setq sunshine-location "Doncaster, UK")
;;   (setq sunshine-show-icons t))


(req-package visual-fill-column)

(provide 'init-gui)
