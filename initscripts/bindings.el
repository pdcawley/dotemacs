(eval-when-compile (require 'autoinsert))
;;; 99bindings.el --- Misc. global key bindings
(require 'pdc-support)
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

(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "S-<end>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "S-<home>") 'beginning-of-buffer)

(global-set-key (kbd "C-c /") 'toggle-window-split)

;;(global-set-key (kbd "C-xC-f") 'ido-find-file)

;; A few from Steve Yegge that seem to make sense
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)
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

(bind-key "C-s" 'isearch-forward-regexp)
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
(when (require 'windmove nil 'noerror)
  (global-set-key (kbd "s-<left>") 'windmove-left)
  (global-set-key (kbd "s-<right>") 'windmove-right)
  (global-set-key (kbd "s-<up>") 'windmove-up)
  (global-set-key (kbd "s-<down>") 'windmove-down))

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

(global-set-key (kbd "C-h V") 'pdc/apropos-variable)
(global-set-key (kbd "C-h A") nil)
(global-set-key (kbd "C-h A v") 'pdc/apropos-variable)
(global-set-key (kbd "C-h A d") 'apropos-documentation)
(global-set-key (kbd "C-h A V") 'apropos-value)
(global-set-key (kbd "C-h A l") 'apropos-library)
(global-set-key (kbd "C-h A f") 'pdc/apropos-function)

(defun pdc/apropos-variable ()
  "My apropos lookup for variables"
  (interactive)
  (let ((current-prefix-arg (not current-prefix-arg)))
    (call-interactively 'apropos-variable)))

(defun pdc/apropos-function (pattern)
  "Apropos lookup for commands and functions"
  (interactive (list (apropos-read-pattern "command or function")))
  (apropos-command pattern t))

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

(req-package buffer-mode
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


(define-key input-decode-map "\eOD" [left])
(define-key input-decode-map "\eOC" [right])
(define-key input-decode-map "\eOA" [up])
(define-key input-decode-map "\eOB" [down])

(define-key input-decode-map "\e[F" [end])
(define-key input-decode-map "\e[D" [S-left])
(define-key input-decode-map "\e[C" [S-right])
(define-key input-decode-map "\e[A" [S-up])
(define-key input-decode-map "\e[B" [S-down])
(define-key input-decode-map "\e[C" [S-right])
(define-key input-decode-map "\e[I" [prior])
(define-key input-decode-map "\e[G" [next])
(define-key input-decode-map "\e[M" [f1])
(define-key input-decode-map "\e[Y" [S-f1])
(define-key input-decode-map "\e[k" [C-f1])
(define-key input-decode-map "\e\e[M" [M-f1])
(define-key input-decode-map "\e[N" [f2])
(define-key input-decode-map "\e[Z" [S-f2])
(define-key input-decode-map "\e[l" [C-f2])
(define-key input-decode-map "\e\e[N" [M-f2])
(define-key input-decode-map "\e[O" [f3])
(define-key input-decode-map "\e[a" [S-f3])
(define-key input-decode-map "\e[m" [C-f3])
(define-key input-decode-map "\e\e[O" [M-f3])
(define-key input-decode-map "\e[P" [f4])
(define-key input-decode-map "\e[b" [S-f4])
(define-key input-decode-map "\e[n" [C-f4])
(define-key input-decode-map "\e\e[P" [M-f4])
(define-key input-decode-map "\e[Q" [f5])
(define-key input-decode-map "\e[c" [S-f5])
(define-key input-decode-map "\e[o" [C-f5])
(define-key input-decode-map "\e\e[Q" [M-f5])
(define-key input-decode-map "\e[R" [f6])
(define-key input-decode-map "\e[d" [S-f6])
(define-key input-decode-map "\e[p" [C-f6])
(define-key input-decode-map "\e\e[R" [M-f6])
(define-key input-decode-map "\e[S" [f7])
(define-key input-decode-map "\e[e" [S-f7])
(define-key input-decode-map "\e[q" [C-f7])
(define-key input-decode-map "\e\e[S" [M-f7])
(define-key input-decode-map "\e[T" [f8])
(define-key input-decode-map "\e[f" [S-f8])
(define-key input-decode-map "\e[r" [C-f8])
(define-key input-decode-map "\e\e[T" [M-f8])
(define-key input-decode-map "\e[U" [f9])
(define-key input-decode-map "\e[g" [S-f9])
(define-key input-decode-map "\e[s" [C-f9])
(define-key input-decode-map "\e\e[U" [M-f9])
(define-key input-decode-map "\e[V" [f10])
(define-key input-decode-map "\e[h" [S-f10])
(define-key input-decode-map "\e[_" [C-f10])
(define-key input-decode-map "\e\e[V" [M-f10])
(define-key input-decode-map "\e[W" [f11])
(define-key input-decode-map "\e[i" [S-f11])
(define-key input-decode-map "\e[u" [C-f11])
(define-key input-decode-map "\e\e[W" [M-f11])
(define-key input-decode-map "\e[X" [f12])
(define-key input-decode-map "\e[j" [S-f12])
(define-key input-decode-map "\e[v" [C-f12])
(define-key input-decode-map "\e\e[X" [M-f12])
(define-key input-decode-map (kbd "A-C-g") (kbd "C-g"))

(global-set-key (kbd "<A-backspace>") (kbd "<backspace>"))
(global-set-key (kbd "H-a") (beginning-of-line))

;; Movement layer.

(global-set-key (kbd "A-s") 'right-char)
(global-set-key (kbd "A-n") 'left-char)
(global-set-key (kbd "A-y") 'previous-line)
(global-set-key (kbd "A-i") 'next-line)
(global-set-key (kbd "A-p") 'move-beginning-of-line)
(global-set-key (kbd "A-c") 'move-end-of-line)
(global-set-key (kbd "A-b") 'scroll-down-command)
(global-set-key (kbd "A-f") 'scroll-up-command)
(global-set-key (kbd "A-C-s") 'right-word)
(global-set-key (kbd "A-C-n") 'left-word)
(global-set-key (kbd "A-C-y") 'backward-paragraph)
(global-set-key (kbd "A-C-i") 'forward-paragraph)
(global-set-key (kbd "A-C-p") 'backward-sentence)
(global-set-key (kbd "A-C-c") 'forward-sentence)
(global-set-key (kbd "A-C-b") 'beginning-of-buffer)
(global-set-key (kbd "A-C-f") 'end-of-buffer)
(global-set-key (kbd "A-M-s") 'forward-sexp)
(global-set-key (kbd "A-M-n") 'backward-sexp)
(global-set-key (kbd "A-M-y") 'backward-list)
(global-set-key (kbd "A-M-i") 'forward-list)
(global-set-key (kbd "A-M-p") 'backward-up-list)
(global-set-key (kbd "A-M-c") 'up-list)
(global-set-key (kbd "A-M-b") 'beginning-of-defun)
(global-set-key (kbd "A-M-f") 'end-of-defun)
(global-set-key (kbd "A-M-e") 'down-list)

(global-set-key (kbd "A-a") 'ack-and-a-half)

(global-set-key (kbd "A-j") 'fastnav-jump-to-char-forward)
(global-set-key (kbd "A-J") 'fastnav-jump-to-char-backward)
(global-set-key (kbd "A-g") 'fastnav-sprint-forward)
(global-set-key (kbd "A-G") 'fastnav-sprint-backward)

(global-set-key (kbd "A-/") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "A-/") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "A-\\") 'isearch-repeat-backward)

(global-set-key (kbd "C-A-k") 'kill-line)
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

;; Multicursor stuff
(req-package multiple-cursors
  :bind
  (("C-S-c" . nil)
   ("C-S-c C-S-c" . mc/edit-lines)
   ("C-S-c C-e" . mc/edit-ends-of-lines)
   ("C-S-c C-a" . mc/edit-beginnings-of-lines)))

;; Bookmark stuff
(bind-key "C-x p S" 'pdc/bookmark-magit-status)

;;

(bind-key "M-'" 'insert-pair)
(bind-key "M-\"" 'insert-pair)
(bind-key "<C-M-backspace>" 'backward-kill-sexp)

(bind-key "C-x B" 'ido-switch-buffer-other-window)
(bind-key "C-x C-e" 'pp-eval-last-sexp)
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

(bind-key "D F" 'describe-face help-map)
(bind-key "D b" 'describe-bindings help-map)

(bind-key "C-. P l" 'package-list-packages)
(bind-key "C-. I" 'toggle-input-method)

;; Tavis Rudd's bindings

(req-package auto-complete)
(req-package comint)
(require 'org)
(req-package fm)

(defvar f2-map (make-sparse-keymap))
(defvar f4-map (make-sparse-keymap))
(defvar f6-map (make-sparse-keymap))
(defvar f7-map (make-sparse-keymap))
(defvar f8-map (make-sparse-keymap))

(bind-key "M-`" 'bm-toggle)


(bind-key "M-=" '(lambda ()
                   (interactive)
                   (set-mark-command (list 4))))

(defun dss/hippie-expand ()
  (interactive)
  (if (not ac-completing)
      (call-interactively 'hippie-expand)
    (ac-expand nil)))

(bind-key "M-/" 'dss/hippie-expand)
(bind-key "M-TAB" 'dabbrev-expand)
(bind-key "C-M-l" 'dss/sync-point-all-windows)

(bind-key "C-x C-v" 'revert-buffer)


(req-package dss-bookmarks-registers
  :bind
  (("C-x r b" . dss/bookmark-jump)
   ("C-x r v" . list-register)))


(bind-key "<f2>" f2-map)
(bind-key "<f2> j" 'windmove-left)
(bind-key "<f2> l" 'windmove-right)
(bind-key "<f2> k" 'windmove-up)
(bind-key "<f2> i" 'windmove-down)
(bind-key "<f2> m" 'flymake-goto-prev-error)
(bind-key "<f2> ," 'flymake-goto-next-error)
(bind-key "<f2> [" 'isearch-forward-at-point)
(bind-key "<f2> =" 'magit-status)
(bind-key "<f2> -" 'magit-diff)

(defun dss/eval-region-or-last-sexp ()
  (interactive)
  (if mark-active
      (call-interactively 'eval-region)
    (call-interactively 'eval-last-sexp)))

(bind-key "<f4>" f4-map)
(bind-key "<f4> ." 'dss/grep-project)
(bind-key "<f4> t" 'etags-select-find-tag-at-point)
(bind-key "<f4> r" 'etags-select-find-tag)
(bind-key "<f4> <space>" 'fixup-whitespace)
(bind-key "<f4> s" 'dss/eval-region-or-last-sexp)
(bind-key "<f4> d" 'dss/eval-defun)
(bind-key "<f4> x" 'dss/magit-or-monky)
(bind-key "<f4> v" 'dss/magit-or-monky)
(bind-key "<f4> q" 'monky-queue)
(bind-key "<f4> 3" 'dss/out-sexp)
(bind-key "<f4> 8" 'dss/out-one-sexp)
(bind-key "<f4> 9" 'paredit-wrap-round)
(bind-key "<f4> 0" 'paredit-close-round-and-newline)
(bind-key "<f4> <f4>" (kbd "TAB"))
(bind-key "<f4> i" 'yank)
(bind-key "<f4> u" 'undo)
(bind-key "<f4> y" 'dss/mark-string)
(bind-key "<f4> m" 'mark-sexp)
(bind-key "<f4> '" (kbd "\""))
(bind-key "<f4> ;" 'goto-last-change)
(bind-key "<f4> <f4>" 'kmacro-end-or-call-macro)

(defvar *dss-iedit-auto-complete-was-on* nil)
(make-variable-buffer-local '*dss-iedit-auto-complete-was-on*)
(req-package iedit
  :ensure t
  :bind ("<f4> e" . dss/iedit-toggle)
  :config
  (progn
    (defun dss/iedit-toggle ()
      (interactive)
      (cond (iedit-mode
             (when *dss-iedit-auto-complete-was-on*
               (setq *dss-iedit-auto-complete-was-on* nil)
               (auto-complete-mode t)))
            (t
             (when auto-complete-mode
               (setq *dss-iedit-auto-complete-was-on* t)
               (auto-complete-mode nil)))
            (iedit-mode)))))

(bind-key "<f4> e" 'dss/iedit-toggle)
(bind-key "<f4> 6" 'dss/backward-string)
(bind-key "<f4> 7" 'dss/forward-string)
(bind-key "<f4> -" 'dss/clojure-run-tests)
(bind-key "<f4> c" 'dss/slime-repl-clear)
(bind-key "<f4> p" 'dss/clojure-jump-to-project)
(bind-key "<f4> j" 'dss/clojure-jump-between-tests-and-code)
(bind-key "<f4> /" 'dss/goto-match-paren)
(bind-key "<f4> ]" 'dss/smex)

;; f6-map
(defun dss/insert-todo ()
  (interactive)
  (insert comment-start)
  (insert "@@TR: "))

(defun open-next-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (open-line 1)))

(defun fm-occur (arg)
  (interactive "soccur: ")
  (occur arg)
  (other-window 1)
  (fm-start))

(bind-key "<f6>" f6-map)

(bind-key "<f6> `" 'open-next-line)
(bind-key "<f6> <f6>" 'open-next-line)
(bind-key "<f6> 1" 'replace-string)
(bind-key "<f6> i" 'dss/insert-todo)
(bind-key "<f6> l" 'linum-mode)
(bind-key "<f6> o" 'fm-occur)
(bind-key "<f6> ;" 'string-rectangle)
(bind-key "<f6> k" 'dss/kill-buffer)

(req-package swiper
  :bind
  (("C-s" . swiper)
   ("C-x 4 C-f" . pdc-find-file-other-window)
   ("C-x 4 f" . pdc-find-file-other-window))
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
                :caller 'counsel-find-file))))


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
      (exit-minibuffer))))

(req-package counsel
  :diminish counsel-mode
  :init
  (progn
    (setq ivy-re-builders-alist
          '((t . ivy--regex-plus))))
  :config
  (progn
    (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
    (counsel-mode)
))

(electric-indent-mode -1)