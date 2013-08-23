(require 'evil)
;;; Fixup C-h for evil doers

(define-key evil-normal-state-map (kbd "C-h") 'evil-backward-char)

(define-key evil-replace-state-map (kbd "C-h") 'evil-replace-backspace)

(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)



