
(require 'evil)

(define-key f7-map [(f7)] nil)
;; (evil-set-toggle-key (kbd "<f7> <f7>"))
(setq evil-default-state 'emacs)

(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'org-mode 'emacs)
(evil-set-initial-state 'jabber-chat-mode 'emacs)
(evil-set-initial-state 'jabber-roster-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)

(setq evil-normal-state-tag
      (propertize "N"
                  'face '(:foreground "#ff0000")))

(setq evil-insert-state-tag
      (propertize "_I_"
                  'face '(:foreground "green")))
(setq evil-motion-state-tag
      (propertize "-M-"
                  'face '(:foreground "cyan")))
(setq evil-emacs-state-tag
      (propertize "*E*"
                  'face '(:foreground "green")))

(evil-define-command dss/jk-to-normal-mode ()
  "Allows to get into 'normal' mode using 'jk'."
  :repeat change
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                           nil 0.5)))
      (cond
       ((null evt)
        (message ""))
       ((and (integerp evt) (char-equal evt ?k))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       ((and (integerp evt) (char-equal evt ?l))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (evil-emacs-state))
       (t ; otherwise
        (setq unread-command-events (append unread-command-events
                                            (list evt))))))))

;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
                            (kbd "M-l") 'org-metaright
                            (kbd "M-h") 'org-metaleft
                            (kbd "M-k") 'org-metaup
                            (kbd "M-j") 'org-metadown
                            (kbd "M-L") 'org-shiftmetaright
                            (kbd "M-H") 'org-shiftmetaleft
                            (kbd "M-K") 'org-shiftmetaup
                            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))

; Adding the binding for the j character, then
; the k is handled on the function
(define-key
  evil-insert-state-map
  "j"
  #'dss/jk-to-normal-mode)

(define-key
  evil-normal-state-map
  ";"
  #'evil-ex)

(define-key f7-map (kbd "<f6>")
  '(lambda ()
     (interactive)
     (evil-mode 1)))

(define-key f7-map "q"
  '(lambda ()
     (interactive)
     (evil-mode -1)))

(define-key f7-map "n"
  '(lambda ()
     (interactive)
     (if (evil-emacs-state-p)
         (evil-normal-state)
       (evil-emacs-state))))

(provide 'dss-evil)
