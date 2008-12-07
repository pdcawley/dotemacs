;;; 99bindings.el --- Misc. global key bindings
;; Some custom global key bindings
(global-set-key (kbd "C-c s")  'calendar)
(global-set-key (kbd "C-c g")  'goto-line)
(global-set-key (kbd "C-c c")  'recompile)
(global-set-key (kbd "C-c a")  'apply-macro-to-region-lines)
(global-set-key (kbd "C-c w")  'woman)
(global-set-key (kbd "C-c o")  'occur)
(global-set-key (kbd "M-d")   'kill-word)
(global-unset-key (kbd "C-z"))
(global-unset-key [insert])
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key (kbd "C-c i") 'set-tab-width)

;; ido -- be powerful!
;;(global-set-key (kbd "C-xC-f") 'ido-find-file)

;; A few from Steve Yegge that seem to make sense
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
(defalias 'qrr 'query-replace-regexp)

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
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)

(windmove-default-keybindings)

;; Lispy
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "C-h c") 'cheat)

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
(require 'tabkey2)
(
