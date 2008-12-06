;;; 01hideshow.el --- Add a hideshow global mode

(require 'vc)
(require 'ediff)
(require 'easy-mmode)
(require 'hideshow)

(defun turn-off-hideshow ()
  (hs-minor-mode -1))

(defun turn-on-hideshow ()
  (and (boundp 'comment-start) comment-start
       (boundp 'comment-end) comment-end
       (hs-minor-mode t)))

(add-hook 'ediff-prepare-buffer-hook 'turn-off-hideshow)
(add-hook 'vc-before-checkin-hook 'turn-off-hideshow)

(easy-mmode-define-global-mode
   global-hs-mode hs-minor-mode turn-on-hideshow)

(global-hs-mode 1)

(define-key hs-minor-mode-map "\M-s" 'hs-toggle-hiding)
