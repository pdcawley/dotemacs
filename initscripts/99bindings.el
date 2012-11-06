;;; 99bindings.el --- Misc. global key bindings
;; Some custom global key bindings
(global-set-key (kbd "C-c s")  'calendar)
(global-set-key (kbd "C-c g")  'goto-line)
(global-set-key (kbd "C-c c")  'recompile)
(global-set-key (kbd "C-c a")  'apply-macro-to-region-lines)
(global-set-key (kbd "C-c w")  'woman)
(global-set-key (kbd "C-c o o")  'occur)
(global-set-key (kbd "M-d")   'kill-word)
(global-unset-key (kbd "C-z"))
(global-unset-key [insert])
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key (kbd "C-c i") 'set-tab-width)

;;(global-set-key (kbd "C-xC-f") 'ido-find-file)

;; A few from Steve Yegge that seem to make sense
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-c C-k") 'kmacro-keymap)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr  'replace-regexp)

;; More, inspired by the Emacs Starter Kit
(defgroup pdc nil
  "Group for customizing pdc/* utility functions"
  :tag "Personal customizations")

(defcustom pdc/spruce-up-hook nil
  "Normal hook run after basic spruce up commands"
  :type 'hook
  :group 'pdc)

(defun pdc/spruce-up-buffer ()
  "Performs a general cleanup on the currrent buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (run-hooks pdc/spruce-up-hook))

(global-set-key (kbd "C-c n") 'pdc/spruce-up-buffer)

(if (fboundp 'ido-goto-symbol)
    (global-set-key (kbd "C-x TAB") 'ido-goto-symbol))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-A-s") 'isearch-forward)
(global-set-key (kbd "C-A-r") 'isearch-backward)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(windmove-default-keybindings)

;; Perly
(require 'cperl-mode)
(define-key cperl-mode-map (kbd "RET") 'newline-and-indent)

;; jabbery
(when (require 'jabber nil 'noerror)
  (define-key jabber-chat-mode-map (kbd "M-RET") 'newline))

;; Lispy
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "A-s") 'save-buffer)

(eval-after-load 'vc
  (define-key vc-prefix-map (kbd "i")
    '(lambda () (interactive)
       (if (not (eq 'Git (vc-backend buffer-file-name)))
           (vc-register)
         (shell-command (format "git add %s" buffer-file-name))
         (message "Staged changes")))))

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Some of my own :)
(global-set-key [(meta up)] 'backward-paragraph)
(global-set-key [(meta down)] 'forward-paragraph)

;; My completions
;; isearch -- be sane!
(define-key isearch-mode-map [backspace] 'isearch-delete-char)

;; Tabkey2 and other expansion related malarkey
;(require 'tabkey2)
;(add-to-list 'tabkey2-modes-that-use-more-tabs 'js2-mode)
;(tabkey2-mode 1)

;; vi-like %
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

(require 'fastnav)
(global-set-key (kbd "M-z") 'fastnav-zap-up-to-char-forward)
(global-set-key (kbd "M-Z") 'fastnav-zap-up-to-char-backward)
(global-set-key (kbd "M-s") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "M-S") 'fastnav-jump-to-char-backward)
(global-set-key (kbd "M-F") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "M-B") 'fastnav-jump-to-char-backward)
(global-set-key (kbd "M-r") 'fastnav-replace-char-forward)
(global-set-key (kbd "M-R") 'fastnav-replace-char-backward)
(global-set-key (kbd "M-i") 'fastnav-insert-at-char-forward)
(global-set-key (kbd "M-I") 'fastnav-insert-at-char-backward)
(global-set-key (kbd "M-j") 'fastnav-execute-at-char-forward)
(global-set-key (kbd "M-J") 'fastnav-execute-at-char-backward)
(global-set-key (kbd "M-k") 'fastnav-delete-char-forward)
(global-set-key (kbd "M-K") 'fastnav-delete-char-backward)
(global-set-key (kbd "M-m") 'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-M") 'fastnav-mark-to-char-backward)
(global-set-key (kbd "M-p") 'fastnav-sprint-forward)
(global-set-key (kbd "M-P") 'fastnav-sprint-backward)

(global-set-key (kbd "C-c |")    'pdc/align)
(global-set-key (kbd "C-c \\")   'pdc/align)
(global-set-key (kbd "C-c C-\\") 'pdc/align)

(global-set-key (kbd "C-c '")   'pdc/quote-behind)
(global-set-key (kbd "C-c C-'") 'pdc/quote-behind)
(global-set-key (kbd "C-c \"")  'pdc/doublequote-behind)

(when (require 'buffer-move nil 'noerror)
  (global-set-key (kbd "C-c C-<left>")  'buf-move-left)
  (global-set-key (kbd "C-c C-<right>") 'buf-move-right)
  (global-set-key (kbd "C-c C-<up>")    'buf-move-up)
  (global-set-key (kbd "C-c C-<down>")  'buf-move-down))

;; File type bindings - By doing this last, my desired bindings win
(update-auto-mode-bindings '(("\\.hs\\'"                 . haskell-mode)
                             ("\\.yml\\'"                . yaml-mode)
                             ("\\.rb\\'"                 . ruby-mode)
                             ("\\(?:^\\|/\\)Rakefile\\'" . ruby-mode)
                             ("\\.gemspec\\'"            . ruby-mode)
                             ("\\.rnc\\'"                . rnc-mode)
                             ("\\.js\\'"                 . js2-mode)
                             ("\\.psgi\\'"               . cperl-mode)))
