;; config.el --- -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'general)
  (require 'use-package))

(add-hook 'after-init-hook #'(lambda ()
                               (interactive)
                               (require 'server)
                               (or (server-running-p)
                                   (server-start))))

;;;
;;; Performance
(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t
          package-native-compile t)
  (message "No native compilation available"))

(setq warning-suppress-types '((comp)))



;;
;; More or less sensible defaults

(global-eldoc-mode 1)
(electric-pair-mode 1)
(add-hook 'before-save-hook 'whitespace-cleanup)
(delete-selection-mode)

(setq-default
 frame-resize-pixelwise t

 indent-tabs-mode nil
 tab-width 4
 fill-column 79

 gnutls-verify-error t
 gnutls-min-prime-bits 2048

 password-cache-expiry nil

 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t
 ediff-window-setup-function 'ediff-setup-windows-plain

 tramp-default-method "ssh"
 tramp-copy-size-limit nil
 tramp-use-ssh-controlmaster-options nil

 vc-follow-symlinks t

 ring-bell-function 'ignore)

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))


;; Revert dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)


;; Revert buffers when the underlying file changed.
(global-auto-revert-mode 1)

;; Spaces, not tabs.
(setq-default indent-tabs-mode nil)

;; #'yes-or-no-p can die in a fire
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (expand-file-name "recentf" pdcmacs-var-directory))

;; savehist mode
(setq history-length 25)
(savehist-mode 1)

;; Don't stick duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling a bit less stuttery
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecises-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)


;; Better support for files with long names
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang files executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Diminish some annoying stuff
(diminish 'eldoc-mode "")

;; sentences do not end with a double space
(setq sentence-end-double-space nil)

;; Enable some stuff
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;
;; Editing stuff

;; trim excess whitespace
(use-package ws-butler
  :diminish ""
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;; parentheses
(electric-pair-mode 1)
(show-paren-mode 1)

(use-package paredit
  :diminish "Ⓟ "
  :hook
  ((lisp-mode scheme-mode racket-mode emacs-lisp-mode) . enable-paredit-mode))

(use-package mwim
  :custom
  (mwim-position-functions '(mwim-code-beginning
                             mwim-line-beginning
                             mwim-comment-beginning
                             +mwim-current-string-beginning
                             +mwim-current-string-end
                             mwim-code-end
                             mwim-line-end))
  :general
  (general-def
    "C-a" 'mwim-beginning
    "C-e" '+mwim-next-ending)
  :config
  (defun +mwim-next-ending ()
    "Move point to the the nearest ending place"
    (interactive "^")
    (mwim-move-to-next-position mwim-end-position-functions #'<))

  (defun +mwim-current-string-beginning ()
    "Return position of the beginning of the current string.
Return nil if not inside a string."
    (let ((syn (syntax-ppss)))
      (and (nth 3 syn)
           (nth 8 syn))))

  (defun +mwim-beginning-of-current-string ()
    "Move point of the beginning of the current string.
If we're not in a string, do nothing."
    (interactive "^")
    (when-let ((string-beg (+mwim-current-string-beginning)))
      (goto-char string-beg)))

  (defun +mwim-current-string-end ()
    "Return position of the end of the current string.
Return nil if not inside a string."
    (mwim-point-at (+mwim-end-of-current-string)))

  (defun +mwim-end-of-current-string ()
    "Move point to the end of the current string.
Do nothing if we're not in a string."
    (interactive "^")
    (when-let ((string-beg (+mwim-current-string-beginning)))
      (goto-char string-beg)
      (forward-sexp)))

  (push '+mwim-current-string-beginning mwim-beginning-position-functions)

  (push '+mwim-current-string-end mwim-end-position-functions))


;;
;; Completion frameworks and such.

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (require 'vertico-directory "extensions/vertico-directory.el")  (vertico-mode 1))

(use-package marginalia
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-mode 1))

(use-package consult
  :general
  ("C-s" 'consult-line)
  (:keymaps 'minibuffer-local-map
        "C-r" 'consult-history)
  :config
  (setq completion-in-region-function #'consult-completion-in-region))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (setq completion-category-defaults nil))

(use-package embark
  :general
  ("C-." #'embark-act)
  ([remap describe-bindings] #'embark-bindings)
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;;; Appearance

(use-package all-the-icons)
(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-minor-modes t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :init
  (add-hook 'after-init-hook 'doom-modeline-init))

;; utility libraries
(use-package dash)
(use-package s)
(use-package f)
(use-package kv)
(use-package ht)

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f)
  (require 'kv)
  (require 'ht))

;; Speedup with auto-compile
(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t))

;; Make `describe-*' screens more helpful

(use-package helpful
  :general
  (:keymaps 'helpful-mode-map
            [remap revert-buffer] #'helpful-update)
  ([remap describe-command] #'helpful-command
   [remap describe-function] #'helpful-callable
   [remap describe-key] #'helpful-key
   [remap describe-symbol] #'helpful-symbol
   [remap describe-variable] #'helpful-variable
   "C-h C" #'helpful-command
   "C-h F" #'helpful-function
   "C-h K" #'describe-keymap)
  :config

  ;; Temporary fix until this all works for Emacs 29 again
  (defvar read-symbol-positions-list nil)
  (defun helpful--autoloaded-p (sym buf)
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix "*.gz" file-name))
      (help-fns--autoloaded-p sym)))


  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines)))))

(use-package elisp-demos
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;; add visual pulse when changing focus, like beacon but built-in
(defun pulse-line (&rest _)
  "Pulse the current line"
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;;;
;;; Windows stuff

(use-package winner
  :init
  (winner-mode 1)
  (setq winner-boring-buffers
        (append winner-boring-buffers
                '("*Completions*"
                  "*Compile-Log*"
                  "*inferior-lisp*"
                  "*Fuzzy Completions*"
                  "*Apropos*"
                  "*Help*"
                  "*cvs*"
                  "*Buffer List*"
                  "*Ibuffer*"
                  "*esh command on file*"))))

;;;
;;; Text wrangling

(defun +align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
if JUSTIFY-RIGHT is non nil justify to the right instead of the left. If AFTER is non-nil, add whitespace to the left instead of the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

(defmacro pdc|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "+align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) ,(if default-after t nil)))))
         (+align-repeat start end ,regexp ,justify-right after)))))

  (defun +align-repeat-decimal (start end)
    "Align a table of numbers on decimal points and dollar signs (both optional)"
    (interactive "r")
    (require 'align)
    (align-regexp start end nil
                  '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                         (repeat . t)
                         (group 1 2)
                         (spacing 1 1)
                         (justify nil t)))
                  nil))


(pdc|create-align-repeat-x "comma"     "," nil t)
(pdc|create-align-repeat-x "semicolon" ";" nil t)
(pdc|create-align-repeat-x "colon"     ":" nil t)
(pdc|create-align-repeat-x "equal"     "=")
(pdc|create-align-repeat-x "math-oper" "[+\\-*/]")
(pdc|create-align-repeat-x "ampersand" "&")
(pdc|create-align-repeat-x "bar"       "|")
(pdc|create-align-repeat-x "left-paren" "(")
(pdc|create-align-repeat-x "right-paren" ")" t)
(pdc|create-align-repeat-x "backslash" "\\\\")


(define-prefix-command 'pdc-leader-map)
(general-def
  "M-m" 'pdc-leader-map)

(general-def
  :prefix "M-m |"
  ""   '(nil :which-key "align")
  ","  '+align-repeat-comma
  ";"  '+align-repeat-semicolon
  ":"  '+align-repeat-colon
  "="  '+align-repeat-equal
  "+"  '+align-repeat-math-oper
  "*"  '+align-repeat-math-oper
  "/"  '+align-repeat-math-oper
  "-"  '+align-repeat-math-oper
  "|"  '+align-repeat-bar
  "("  '+align-repeat-left-paren
  ")"  '+align-repeat-right-paren
  "\\" '+align-repeat-backslash)

(define-prefix-command 'pdc-windows-key-map)

(general-def
 :keymaps 'pdc-windows-key-map
 "u" 'winner-undo
 "n" 'windmove-down
 "p" 'windmove-up
 "b" 'windmove-left
 "f" 'windmove-right)

(general-def
  "M-m w" 'pdc-windows-key-map)

;;; Org-mode and friends

(use-package org)
(use-package org-contrib)
(use-package org-roam
  :custom
  (org-roam-directory (file-truename (expand-file-name "~/Documents/RoamNotes/")))
  :init
  (setq org-roam-v2-ack t)
  :general
  (:prefix "M-m n"
   ""  '(nil :which-key "notes")
   "l" 'org-roam-buffer-toggle
   "f" 'org-roam-node-find
   "g" 'org-roam-graph
   "i" 'org-roam-node-insert
   "c" 'org-roam-capture

   "j" 'org-roam-dailies-capture-today))

;;; Magit
(use-package magit
  :general
  (:prefix "M-m g"
           "" '(nil :which-key "Git")
           "s" 'magit-status))

(require 'pdcmacs-hugo-support)
