;; -*- lexical-binding: t -*-
(eval-when-compile (require 'autoinsert))
;;; 99bindings.el --- Misc. global key bindings
(require 'pdc-support)
(require 'init-leaders)
(require 'general)

;; Some custom global key bindings

;; (global-set-key (kbd "M-d")   'kill-word)
(global-unset-key (kbd "C-z"))
(global-unset-key [insert])
(global-set-key [M-insert] 'overwrite-mode)
(global-set-key (kbd "C-c i") 'set-tab-width)

(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "S-<end>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "S-<home>") 'beginning-of-buffer)

(global-set-key (kbd "C-c /") 'toggle-window-split)

;;(global-set-key (kbd "C-xC-f") 'ido-find-file)

(defun pdc/execute-extended-command-short (prefixarg &optional function)
  "A massively simplified execute-extended-command"
  (interactive "P\nCcommand: ")
  (setq this-command function)
  (setq real-this-command function)
  (let ((prefix-arg prefixarg))
    (command-execute function 'record)))



;; A few from Steve Yegge that seem to make sense
(global-set-key (kbd "C-x C-m") 'pdc/execute-extended-command-short)
(global-set-key (kbd "C-c C-m") 'pdc/execute-extended-command-short)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-c C-k") 'kmacro-keymap)
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

;; More, inspired by the Emacs Starter Kit

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

(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-A-s" 'isearch-forward)
(bind-key "C-A-r" 'isearch-backward)

(bind-key "C-c y y" 'bury-buffer)
(bind-key "C-. y" 'bury-buffer)
(bind-key "C-c r" 'revert-buffer)

;; (windmove-default-keybindings)

;; Perly
(require 'cperl-mode)
(define-key cperl-mode-map (kbd "RET") 'newline-and-indent)

;; windmove


;; Lispy
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)
(define-key lisp-mode-shared-map (kbd "C-\\") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "A-s") 'save-buffer)


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

(defun pdc/apropos-variable ()
  "My apropos lookup for variables"
  (interactive)
  (let ((current-prefix-arg (not current-prefix-arg)))
    (call-interactively 'apropos-variable)))

(defun pdc/apropos-function (pattern)
  "Apropos lookup for commands and functions"
  (interactive (list (apropos-read-pattern "command or function")))
  (apropos-command pattern t))

(with-eval-after-load 'general
  (general-define-key :keymaps 'help-map :prefix "A"
    "f" 'pdc/apropos-function
    "l" 'apropos-library
    "V" 'apropos-value
    "d" 'apropos-documentation
    "v" 'pdc/apropos-variable
    "I" 'info-apropos))

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

(req-package fastnav
  :bind
  (("M-z" . fastnav-zap-up-to-char-forward)
   ("M-Z" . fastnav-zap-up-to-char-backward)
   ("M-S" . fastnav-jump-to-char-backward)
   ("M-F" . fastnav-jump-to-char-forward)
   ("M-B" . fastnav-jump-to-char-backward)
   ("M-r" . fastnav-replace-char-forward)
   ("M-R" . fastnav-replace-char-backward)
   ("M-i" . fastnav-insert-at-char-forward)
   ("M-I" . fastnav-insert-at-char-backward)
   ("M-j" . fastnav-execute-at-char-forward)
   ("M-J" . fastnav-execute-at-char-backward)
   ("M-k" . fastnav-delete-char-forward)
   ("M-K" . fastnav-delete-char-backward)
   ("M-M" . fastnav-mark-to-char-backward)
   ("M-p" . fastnav-sprint-forward)
   ("M-P" . fastnav-sprint-backward)))

(global-set-key (kbd "C-c |")    'pdc/align)
(global-set-key (kbd "C-c \\")   'pdc/align)
(global-set-key (kbd "C-c C-\\") 'pdc/align)
(global-set-key (kbd "C-c '")   'pdc/quote-behind)
(global-set-key (kbd "C-c C-'") 'pdc/quote-behind)
(global-set-key (kbd "C-c \"")  'pdc/doublequote-behind)

(req-package buffer-move
  :bind
  (("C-c C-<left>"  . buf-move-left)
   ("C-c C-<right>" . buf-move-right)
   ("C-c C-<up>"    . buf-move-up)
   ("C-c C-<down>"  . buf-move-down)))



;; File type bindcings - By doing this last, my desired bindings win
(update-auto-mode-bindings '(("\\.hs\\'"                 . haskell-mode)
                             ("\\.yml\\'"                . yaml-mode)
                             ("\\.rb\\'"                 . ruby-mode)
                             ("\\(?:^\\|/\\)Rakefile\\'" . ruby-mode)
                             ("\\.gemspec\\'"            . ruby-mode)
                             ("\\.rnc\\'"                . rnc-mode)
                             ("\\.js\\'"                 . js2-mode)
                             ("\\.psgi\\'"               . perl-mode)
                             ("\\.t\\'"                  . perl-mode)))


(define-key input-decode-map (kbd "A-C-g") (kbd "C-g"))

(global-set-key (kbd "<A-backspace>") (kbd "<backspace>"))

(global-set-key (kbd "H-a") (beginning-of-line))

;; Movement layer.

;; (global-set-key (kbd "A-s") 'right-char)
;; (global-set-key (kbd "A-n") 'left-char)
;; (global-set-key (kbd "A-y") 'previous-line)
;; (global-set-key (kbd "A-i") 'next-line)
;; (global-set-key (kbd "A-p") 'move-beginning-of-line)
;; (global-set-key (kbd "A-c") 'move-end-of-line)
;; (global-set-key (kbd "A-b") 'scroll-down-command)
;; (global-set-key (kbd "A-f") 'scroll-up-command)
;; (global-set-key (kbd "A-C-s") 'right-word)
;; (global-set-key (kbd "A-C-n") 'left-word)
;; (global-set-key (kbd "A-C-y") 'backward-paragraph)
;; (global-set-key (kbd "A-C-i") 'forward-paragraph)
;; (global-set-key (kbd "A-C-p") 'backward-sentence)
;; (global-set-key (kbd "A-C-c") 'forward-sentence)
;; (global-set-key (kbd "A-C-b") 'beginning-of-buffer)
;; (global-set-key (kbd "A-C-f") 'end-of-buffer)
;; (global-set-key (kbd "A-M-s") 'forward-sexp)
;; (global-set-key (kbd "A-M-n") 'backward-sexp)
;; (global-set-key (kbd "A-M-y") 'backward-list)
;; (global-set-key (kbd "A-M-i") 'forward-list)
;; (global-set-key (kbd "A-M-p") 'backward-up-list)
;; (global-set-key (kbd "A-M-c") 'up-list)
;; (global-set-key (kbd "A-M-b") 'beginning-of-defun)
;; (global-set-key (kbd "A-M-f") 'end-of-defun)
;; (global-set-key (kbd "A-M-e") 'down-list)

;; (global-set-key (kbd "A-a") 'ack-and-a-half)

;; (global-set-key (kbd "A-j") 'fastnav-jump-to-char-forward)
;; (global-set-key (kbd "A-J") 'fastnav-jump-to-char-backward)
;; (global-set-key (kbd "A-g") 'fastnav-sprint-forward)
;; (global-set-key (kbd "A-G") 'fastnav-sprint-backward)

;; (global-set-key (kbd "A-/") 'isearch-forward-regexp)
;; (define-key isearch-mode-map (kbd "A-/") 'isearch-repeat-forward)
;; (define-key isearch-mode-map (kbd "A-\\") 'isearch-repeat-backward)

;; (global-set-key (kbd "C-A-k") 'kill-line)

(req-package compile
  :bind
  (:map compilation-mode-map
    ("<A-M-mouse-1>" . compile-goto-error)))

(global-set-key (kbd "A--") 'negative-argument)

(defun pdc/add-movement-keys ()
  (local-set-key (kbd "i") 'next-line)
  (local-set-key (kbd "y") 'previous-line))

(mapc (lambda (each) (add-hook each 'pdc/add-movement-keys))
      '(gnus-summary-mode-hook gnus-group-mode-hook))

;; Grab a prefix mode hotkey prefix

(global-set-key (kbd "C-c C-c") nil)


;; Bookmark stuff
(bind-key "C-x p S" 'pdc/bookmark-magit-status)

;;

(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)
(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(bind-key "C-x B" 'ido-switch-buffer-other-window)
;; (bind-key "C-x C-e" 'pp-eval-last-sexp)
(bind-key "C-c <tab>" 'ff-find-other-file)

(defun do-eval-buffer ()
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun scratch ()
  (interactive)
  (let ((current-mode major-mode))
    (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (goto-char (point-min))
    (when (looking-at ";")
      (forward-line 4)
      (delete-region (point-min) (point)))
    (goto-char (point-max))
    (if (memq current-mode lisp-modes)
        (funcall current-mode))))

(bind-key "C-c e b" 'do-eval-buffer)
(bind-key "C-c e c" 'cancel-debug-on-entry)
(bind-key "C-c e d" 'debug-on-entry)
(bind-key "C-c e e" 'toggle-debug-on-error)
(bind-key "C-c e f" 'emacs-lisp-byte-compile-and-load)
(bind-key "C-c e j" 'emacs-lisp-mode)
(bind-key "C-c e l" 'find-library)
(bind-key "C-c e r" 'eval-region)
(bind-key "C-c e s" 'scratch)

(bind-key "<C-return>" 'other-window)

(defun collapse-or-expand ()
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" 'collapse-or-expand)

(defadvice async-shell-command (before uniquify-running-shell-command
                                       activate)
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buffer (let ((proc (get-buffer-process buf)))
                 (if (and proc (eq 'run (process-state proc)))
                     (with-current-buffer buf (rename-uniquely)))))))

(bind-key "M-!" 'async-shell-command)
(bind-key "M-/" 'dabbrev-expand)
(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)

(bind-key "C-. [" 'align-code)
(bind-key "M-`" 'other-frame)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" 'mark-line)


(defun mark-sentence (&optional arg)
  (interactive "p")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(bind-key "C-. P l" 'package-list-packages)
(bind-key "C-. I" 'toggle-input-method)

;; Tavis Rudd's bindings

;; (req-package auto-complete)
;; (req-package comint)
;; (require 'org)
;; (req-package fm)

;; (defvar f2-map (make-sparse-keymap))
;; (defvar f4-map (make-sparse-keymap))
;; (defvar f6-map (make-sparse-keymap))
;; (defvar f7-map (make-sparse-keymap))
;; (defvar f8-map (make-sparse-keymap))

;; (bind-key "M-`" 'bm-toggle)


;; (bind-key "M-=" '(lambda ()
;;                    (interactive)
;;                    (set-mark-command (list 4))))

;; (defun dss/hippie-expand ()
;;   (interactive)
;;   (if (not ac-completing)
;;       (call-interactively 'hippie-expand)
;;     (ac-expand nil)))

;; (bind-key "M-/" 'dss/hippie-expand)
;; (bind-key "M-TAB" 'dabbrev-expand)
;; (bind-key "C-M-l" 'dss/sync-point-all-windows)

;; (bind-key "C-x C-v" 'revert-buffer)

;; (req-package list-register
;;   :bind
;;    ("C-x r v" . list-register))


;; (defun dss/eval-region-or-last-sexp ()
;;   (interactive)
;;   (if mark-active
;;       (call-interactively 'eval-region)
;;     (call-interactively 'eval-last-sexp)))

;; (general-define-key
;;  :prefix "<f4>"
;;   ""         f4-map
;;   "."       'dss/grep-project
;;   "t"       'etags-select-find-tag-at-point
;;   "r"       'etags-select-find-tag
;;   "<space>" 'fixup-whitespace
;;   "s"       'dss/eval-region-or-last-sexp
;;   "d"       'dss/eval-defun
;;   "x"       'dss/magit-or-monky
;;   "v"       'dss/magit-or-monky
;;   "q"       'monky-queue
;;   "3"       'dss/out-sexp
;;   "8"       'dss/out-one-sexp
;;   "9"       'paredit-wrap-round
;;   "0"       'paredit-close-round-and-newline
;;   "i"       'yank
;;   "u"       'undo
;;   "y"       'dss/mark-string
;;   "m"       'mark-sexp
;;   "'"       (kbd "\"")
;;   ";"       'goto-last-change
;;   "<f4>"    'kmacro-end-or-call-macro)

;; (defvar *dss-iedit-auto-complete-was-on* nil)
;; (make-variable-buffer-local '*dss-iedit-auto-complete-was-on*)
;; (req-package iedit
;;   :bind ("<f4> e" . dss/iedit-toggle)
;;   :config
;;   (progn
;;     (defun dss/iedit-toggle ()
;;       (interactive)
;;       (cond (iedit-mode
;;              (when *dss-iedit-auto-complete-was-on*
;;                (setq *dss-iedit-auto-complete-was-on* nil)
;;                (auto-complete-mode t)))
;;             (t
;;              (when auto-complete-mode
;;                (setq *dss-iedit-auto-complete-was-on* t)
;;                (auto-complete-mode nil)))
;;             (iedit-mode)))))

;; (bind-key "<f4> e" 'dss/iedit-toggle)
;; (bind-key "<f4> 6" 'dss/backward-string)
;; (bind-key "<f4> 7" 'dss/forward-string)
;; (bind-key "<f4> -" 'dss/clojure-run-tests)
;; (bind-key "<f4> c" 'dss/slime-repl-clear)
;; (bind-key "<f4> p" 'dss/clojure-jump-to-project)
;; (bind-key "<f4> j" 'dss/clojure-jump-between-tests-and-code)
;; (bind-key "<f4> /" 'dss/goto-match-paren)
;; (bind-key "<f4> ]" 'dss/smex)

;; ;; f6-map
;; (defun dss/insert-todo ()
;;   (interactive)
;;   (insert comment-start)
;;   (insert "@@TR: "))

;; (defun open-next-line ()
;;   (interactive)
;;   (save-excursion
;;     (end-of-line)
;;     (open-line 1)))

;; (defun fm-occur (arg)
;;   (interactive "soccur: ")
;;   (occur arg)
;;   (other-window 1)
;;   (fm-start))


;; (general-define-key
;;  :prefix "<f6>"
;;  ""     '(nil :which-key "+lines")
;;  "`"    'open-next-line
;;  "<f6>" 'open-next-line
;;  "1"    'replace-string
;;  "i"    'dss/insert-todo
;;  "l"    'linum-mode
;;  "o"    'fm-occur
;;  ";"    'string-rectangle
;;  "k"    'kill-this-buffer)

(electric-indent-mode -1)

(define-prefix-command 'pdc/describe nil "describe")

(general-define-key :keymaps 'pdc/describe
  "b" 'describe-bindings
  "C" '(finder-commentary :which-key "commentary")
  "F" 'describe-face
  "i" 'describe-input-method
  "k" 'describe-key
  "L" 'describe-language-environment
  "m" 'describe-mode
  "o" 'describe-symbol
  "P" 'describe-package
  "s" 'describe-syntax
  "T" 'describe-text-properties
  "t" 'describe-theme
  )

(define-key help-map (kbd "D") 'pdc/describe)

(general-define-key :keymaps 'help-map
  "C-h" nil
  "u" 'man
  ;; Unbind old 'describe' bindings
  "b" nil
  "I" nil
  "L" nil

  ;; "D"   pdc/describe
  ;; "D b" 'describe-bindings
  ;; "D C" '(finder-commentary :which-key "commentary")
  ;; "D F" 'describe-face
  ;; "D i" 'describe-input-method
  ;; "D k" 'describe-key
  ;; "D L" 'describe-language-environment
  ;; "D m" 'describe-mode
  ;; "D o" 'describe-symbol
  ;; "D P" 'describe-package
  ;; "D s" 'describe-syntax
  ;; "D T" 'describe-text-properties
  ;; "D t" 'describe-theme
  )

(req-package s)
(req-package dash)

;; (with-eval-after-load
;;     (--each (list (concat leader-key " h")
;;                   "<f1>" "C-h")
;;       (cl-flet ((with-prefix (key)
;;                              (s-join " " (list it key))))
;;         (which-key-declare-prefixes
;;           (with-prefix "D") "describe"
;;           (with-prefix "A") "apropos"
;;           (with-prefix "e") "elisp"))))

(with-eval-after-load 'hydra
  (pdc|with-leader
   "j l" 'dss/goto-line
   "j $" 'move-end-of-line
   "j >" 'end-of-buffer
   "j <" 'beginning-of-buffer
   "j ^" 'beginning-of-line
   "j a" 'back-to-indentation

   "h" (list help-map :which-key "help")
   "w /" 'toggle-window-split

   "." '(:ignore t :which-key "misc")
   ". r" 'recompile
   ". s" 'pdc/spruce-up-buffer

   "k" (list
        (defhydra hydra-kill (nil nil :color red)
          "Kill"
          (";" kill-comment "comment")
          ("<tab>" delete-indentation "indentation")
          ("L" kill-whole-line "whole line")
          ("R" kill-rectangle "rectangle" :color blue)
          ("S" kill-sentence "sentence")
          ("l" kill-line "line")
          ("p" kill-paragraph "paragraph")
          ("r" kill-region "region" :color blue)
          ("s" kill-sexp "sexp")
          ("v" kill-visual-line "visual line")
          ("w" kill-word "word"))
        :which-key "+kill")

   ;; "k -" '(nil :which-key "backward")
   ;; "k - p" 'backward-kill-paragraph
   ;; "k - S" 'backward-kill-sentence
   ;; "k - s" 'backward-kill-sexp
   ;; "k - w" 'backward-kill-word
   ;; "k - u" 'kill-backward-up-list

   "K" (list kmacro-keymap :which-key "kmacro")

   "r" '(:ignore t :which-key "replace")
   "r q" 'query-replace-regexp
   "r R" 'replace-rectangle))

(global-set-key [remap dabbrev-expand] 'hippie-expand)


(provide 'init-bindings)

