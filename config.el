;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'general)
  (require 'use-package))

(defun start-server-after-init ()
  (interactive)
  (require 'server)
  (unless (server-running-p)
    (server-start)
    (midnight-mode +1)))

(use-package emacs
  :hook
  (after-init . start-server-after-init))




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

;; (global-eldoc-mode 1)
(electric-pair-mode 1)
(add-hook 'before-save-hook 'whitespace-cleanup)
(delete-selection-mode)

(setopt
 frame-resize-pixelwise t

 indent-tabs-mode nil
 tab-width 4
 fill-column 79

 gnutls-verify-error t
 gnutls-min-prime-bits 2048

 password-cache-expiry nil

 track-eol t

 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 apropos-do-all t
 require-final-newline t

 tramp-default-method "ssh"
 tramp-copy-size-limit nil
 tramp-use-ssh-controlmaster-options nil

 vc-follow-symlinks t

 ring-bell-function 'ignore)

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

;; Autorevert stuff

(use-package autorevert
  :straight (autorevert :type built-in)
  :custom
  ;; Revert dired and other buffers
  (global-auto-revert-non-file-buffers t)
  :config
  ;; Revert buffers when the underlying file changed.
  (global-auto-revert-mode 1))

;; Spaces, not tabs.
(setopt indent-tabs-mode nil
              completions-detailed t
              read-minibuffer-restore-windows nil
              mode-line-compact 'long)

;; #'yes-or-no-p can die in a fire
(setopt use-short-answers t)
;; Turn on recentf mode
(setq recentf-save-file (expand-file-name "recentf" pdcmacs-var-directory))

;; savehist mode
(setq history-length 25
      history-delete-duplicates t)
(savehist-mode 1)

;; Don't stick duplicates in kill-ring
(setopt kill-do-not-save-duplicates t)

;; Make scrolling a bit less stuttery
(setq auto-window-vscroll nil)
(setopt fast-but-imprecises-scrolling t)
(setopt scroll-conservatively 101)
(setopt scroll-margin 0)
(setopt scroll-preserve-screen-position t)


;; Better support for files with long names
(setopt bidi-paragraph-direction 'left-to-right)
(setopt bidi-inhibit-bpa t)
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
  :diminish
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;; parentheses
(electric-pair-mode 1)
(show-paren-mode 1)

(use-package lispy
  :after paredit
  :general
  (:keymaps 'lispy-mode-map
            "M-m" nil))                 ; Lispy tries to use our leader key.

(defun pdc/prioritise-paredit-bindings ()
  (push (assoc 'paredit-mode minor-mode-map-alist)
        minor-mode-overriding-map-alist))

(use-package paredit
  :diminish "Ⓟ "
  :config
  (general-define-key
   :keymaps 'paredit-mode-map
   "DEL" 'pdc/paredit-backward-delete
   "("   'pdc/paredit-open-parenthesis
   ")"   'paredit-close-round-and-newline
   "M-)" 'paredit-close-round
   "C-M-l" 'paredit-recenter-on-sexp
   "C-M-s" 'paredit-backward-up
   "M-I" 'paredit-splice-sexp
   "]" 'paredit-close-square-and-newline
   "}" 'paredit-close-curly-and-newline
   ";" 'pdc/paredit-semicolon)

  (defun pdc/paredit-backward-delete ()
    (interactive)
    (if mark-active
        (call-interactively 'delete-region)
      (paredit-backward-delete)))

  (defun pdc/paredit-semicolon (&optional n)
    (interactive "P")
    (when (looking-at-p "  +\(")
      (search-forward "(")
      (backward-char))
    (cond ((and n (not (= 1 n)))
           (paredit-semicolon n))
          ((and (equal last-command this-command)
                (looking-back "; " 2))
           (undo)
           (self-insert-command 1))
          ((or (looking-back ";" 1)
               (and (looking-at-p "[[:blank:]]*$")
                    (not (save-excursion
                           (beginning-of-line)
                           (looking-at-p "[[:blank:]]*$")))))

           (self-insert-command (or n 1)))

          ((and (not mark-active)
                (looking-at-p "^[[:blank:]]*$"))
           (insert ";;; "))
          ((and (not mark-active)
                (save-excursion
                  (beginning-of-line)
                  (looking-at-p "[[:blank:]]*$")))
           (insert ";; "))
          (t (paredit-semicolon n))))

  (defun pdc/in-string-p ()
    (eq 'string (syntax-ppss-context (syntax-ppss))))

  (defun pdc/in-comment-p ()
    (eq 'comment (syntax-ppss-context (syntax-ppss))))

  (defun pdc/paredit-open-parenthesis (&optional n)
    (interactive "P")
    (cond ((and (looking-back "\(" 1)
                (looking-at "\)"))
           (paredit-open-parenthesis n))
          ((equal last-command this-command)
           (undo)
           (insert " ")
           (backward-char 1)
           (paredit-open-parenthesis n))
          ((and (not (or mark-active (pdc/in-string-p)))
                (looking-at-p "[\(a-z\"#\\[{]"))
           (mark-sexp)
           (paredit-open-parenthesis n)
           (when (looking-at-p "[\(\"#\\[{]")
             (save-excursion (insert " "))))
          (t (paredit-open-parenthesis n))))

  (defvar +paredit--post-close-keymap (make-sparse-keymap))
  (general-define-key :keymaps '+paredit--post-close-keymap
                      "SPC" (lambda () (interactive) (just-one-space -1))
                      "RET" (lambda () (interactive))
                      "DEL" (lambda ()
                              (interactive)
                              (delete-all-space t)))

  (defun pdc/enable-post-close-keymap ()
    (set-transient-map +paredit--post-close-keymap))

  (dolist (closer '(paredit-close-square-and-newline
                    paredit-close-round-and-newline
                    paredit-close-curly-and-newline
                    paredit-close-angled-and-newline))
    (advice-add closer :after 'pdc/enable-post-close-keymap))

  (defun +paredit-maybe-close-doublequote-and-newline (&optional n)
    (cond ((and (paredit-in-string-p)
                (eq (point) (- (paredit-enclosing-string-end) 1)))
           (forward-char)
           (let ((comment.point (paredit-find-comment-on-line)))
             (newline)
             (if comment.point
                 (save-excursion
                   (forward-line -1)
                   (end-of-line)
                   (indent-to (cdr comment.point))
                   (insert (car comment.point))))
             (lisp-indent-line)
             (paredit-ignore-sexp-errors (indent-sexp))
             (pdc/enable-post-close-keymap)
             t))
          (t nil)))

  (advice-add 'paredit-doublequote :before-until '+paredit-maybe-close-doublequote-and-newline)

  :hook
  (paredit-mode . pdc/prioritise-paredit-bindings)
  (paredit-mode . (lambda () (if (fboundp 'lispy-mode) (lispy-mode))))
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

(defgroup pdcmacs nil
  "Pdcmacs customization.")

(defcustom pdc-icon t
  "Display icons or not."
  :group 'pdcmacs
  :type 'boolean)


(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and window-system
       pdc-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))


;;
;; Completion frameworks and such.

(use-package vertico
  :custom
  (vertico-cycle t)
  :hook
  (after-init . vertico-mode))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-light nil)))


(use-package consult
  :general
  ([remap isearch-forward] 'consult-line
   [remap Info-search] 'consult-info
   [remap imenu] 'consult-imenu
   [remap recentf-open-files] 'consult-recent-file

   "C-x M-:" 'consult-complex-command
   "C-x b" 'consult-buffer
   "C-x 4 b" 'consult-buffer-other-window
   "C-x 5 b" 'consult-buffer-other-frame
   "C-x r b" 'consult-bookmark
   "C-x p b" 'consult-project-buffer

   "M-#" 'consult-register-load
   "M-'" 'consult-register-store
   "C-M-#" 'consult-register

   "M-y" 'consult-yank-pop

   :keymaps 'search-map
   "d" 'consult-find
   "D" 'consult-locate
   "g" 'consult-grep
   "G" 'consult-git-grep
   "r" 'consult-ripgrep
   "l" 'consult-line
   "L" 'consult-line-multi
   "k" 'consult-keep-lines
   "u" 'consult-focus-lines
   "e" 'consult-isearch-history

   :keymaps 'isearch-mode-map
   "M-e" 'consult-isearch-history
   "M-s e" 'consult-isearch-history
   "M-s l" 'consult-line
   "M-s L" 'consult-line-multi
   :keymaps 'minibuffer-local-map
   "C-s" `(,(lambda ()
              "Insert the current symbol"
              (interactive)
              (insert (save-excursion
                        (set-buffer (window-buffer (minibuffer-selected-window)))
                        (or (thing-at-point 'symbol t) ""))))
           :which-key "insert-current-symbol")
   "M-s" 'consult-history
   "M-r" 'consult-history)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setopt register-preview-delay 0.5
          register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (consult-customize
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any)))


(use-package orderless
  :custom
  (completion-styles '(substring orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  :config
  (setq completion-category-defaults nil))

(use-package embark
  :general
  ("C-." #'embark-act)
  ([remap describe-bindings] #'embark-bindings)
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Window management settings

(setopt switch-to-buffer-in-dedicated-window 'pop
        switch-to-buffer-obey-display-actions t)

(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)))
(add-to-list 'display-buffer-alist
             '("\\*Compilation\\*"
               display-buffer-reuse-window))
(add-to-list 'display-buffer-alist
             '("\\*info\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters
                (no-delete-other-windows . t))))

(require 'rx)
(add-to-list 'display-buffer-alist
             `(,(rx (| "*xref*"
                       "*grep*"
                       "*Occur*"))
               display-buffer-reuse-window
               (inhibit-same-window . nil)))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*" display-buffer-no-window
               (allow-no-window . t)))


(use-package embark-consult)
(use-package consult-dir)
(use-package ace-window)
(use-package 0x0)

;;; Appearance

(use-package all-the-icons)
;; (use-package doom-modeline
;;   :custom
;;   (doom-modeline-height 15)
;;   (doom-modeline-bar-width 6)
;;   (doom-modeline-minor-modes t)
;;   (doom-modeline-buffer-file-name-style 'truncate-except-project)
;;   :init
;;   (add-hook 'after-init-hook 'doom-modeline-init))

;; utility libraries
(use-package dash
  :config
  (dash-enable-font-lock))
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
  ;; (defvar read-symbol-positions-list nil)
  ;; (defun helpful--autoloaded-p (sym buf)
  ;;   (-when-let (file-name (buffer-file-name buf))
  ;;     (setq file-name (s-chop-suffix "*.gz" file-name))
  ;;     (help-fns--autoloaded-p sym)))


  ;; (defun helpful--skip-advice (docstring)
  ;;   "Remove mentions of advice from DOCSTRING."
  ;;   (let* ((lines (s-lines docstring))
  ;;          (relevant-lines
  ;;           (--take-while
  ;;            (not (or (s-starts-with-p ":around advice:" it)
  ;;                     (s-starts-with-p "This function has :around advice:" it)))
  ;;            lines)))
  ;;     (s-trim (s-join "\n" relevant-lines))))
  )

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
  :straight (winner :type built-in)
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

(require 'pdcmacs-global-bindings)

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
(pdc|create-align-repeat-x "quote" "['`]'")


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
  "\\" '+align-repeat-backslash
  "'"  '+align-repeat-quote
  "`"  '+align-repeat-quote)

(use-package macrostep
  :general
  ("M-m e e" 'macrostep-expand))

;;; Repeat mode stuffs

(use-package repeat
  :custom
  (repeat-echo-function #'ignore)
  :config
  (advice-add 'repeat-post-hook :after
            (defun repeat-help--which-key-popup ()
              (if-let ((cmd (or this-command real-this-command))
                       (keymap (or repeat-map
                                   (repeat--command-property 'repeat-map))))
                  (run-at-time
                   0 nil
                   (lambda ()
                     (which-key--create-buffer-and-show nil (symbol-value keymap))))
                (which-key--hide-popup))))
  (repeat-mode t))

(use-package smerge-mode
  :after which-key
  :straight (smerge-mode :type built-in)
  :custom
  (smerge-auto-leave nil)
  :general
  (:keymaps 'smerge-mode-map
            "M-m m" '(smerge-basic-map :wk "merge" :keymap t))
  :config
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map 'smerge-basic-map)))
   smerge-basic-map))

;;; Magit
(use-package magit
  :general
  (:prefix "M-m g"
           "" '(nil :which-key "Git")
           "s" 'magit-status
           "l" 'magit-log))


(use-package corfu
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (tab-first-completion 'word)
  (completion-cycle-threshold nil)      ; Always show candidates in menu

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)

  (corfu-quit-at-boundary 'separator)

  :hook
  (eshell-history-mode . +eshell-history-mode-setup-completion)
  (lsp-completion-mode . +lsp-mode-setup-completion)

  :general
  (:keymaps 'corfu-map
            "M-SPC" 'corfu-insert-separator
            "RET"   'corfu-insert
            "S-<return>" 'corfu-insert
            "M-m" '+corfu-move-to-minibuffer)

  :init
  ;; TODO: Write a function to attach to tab that first completes a common prefix and, on second hit, inserts the current selection

  (defun +corfu-move-to-minibuffer ()
    (interactive)
    (let (completion-cycle-threshold completion-cycling)
      (apply #'consult-completion-in-region completion-in-region--data)))

  (defun +lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  (defun +eshell-history-mode-setup-completion ()
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode t))

  (global-corfu-mode))

(use-package corfu-terminal
  :if
  (not window-system)
  :init
  (corfu-terminal-mode t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yaml)
(use-package flycheck-yamllint)

(use-package clipetty
  :diminish
  :hook (after-init . global-clipetty-mode))

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :functions (yas-load-directory)
  :diminish (yas-minor-mode . " ⓨ")
  :mode ("/yasnippet/snippets" . snippet-mode)
  :init
  (defvar yas-global-mode nil)
  (setopt yas-triggers-in-field t
          yas-wrap-around-region t
          yas-prompt-functions '(yas-completing-prompt))
  (defvar pdc-snippet-dirs (-filter 'file-directory-p
                                    (list (expand-file-name "snippets/" user-emacs-directory)
                                          (expand-file-name "~/.config/snippets"))))


  (setq yas-snippet-dirs pdc-snippet-dirs))


(use-package consult-yasnippet
  :after (consult yasnippet)
  :general
  ("M-g y" 'consult-yasnippet))


;; (use-package yasnippets-orgmode
;;   :after org-mode)
(use-package yasnippets
  :after yasnippet

  :init
  (setq yas-snippet-dirs (cons (straight--el-get-package-directory 'yasnippets)
                               pdc-snippet-dirs))
  (yas-global-mode t))


;; Need some thought about visual line modes
(use-package emacs
  :hook
  (((text-mode org-mode) . visual-line-mode)
   (prog-mode . toggle-word-wrap))
  :custom
  (truncate-lines nil))

(use-package visual-fill-column
  :defer nil
  :hook
  ((text-mode org-mode) . visual-fill-column-mode)
  :config
  (setq visual-fill-column-enable-sensible-window-split t))


(for-terminal
  (xterm-mouse-mode 1))

(use-package powerline
  :init
  (powerline-default-theme))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Movement/jumping
(defvar pdc-jump-map (make-sparse-keymap))

(use-package avy
  :general
  (pdcmacs-jump-def
    "j" 'avy-goto-char-timer
    "b" 'avy-goto-char
    "'" 'avy-goto-char-2
    "w" 'avy-goto-word-1))

(use-package imenu
  :general
  (pdcmacs-leader-def :infix "j"
    "i" 'imenu)
  :hook
  (font-lock-mode .  pdc/try-to-add-imenu)
  :custom
  (imenu-sort-function 'imenu--sort-by-name)
  :init
  (defun pdc/try-to-add-imenu ()
      "Add Imenu to modes that have font-lock-mode activated."
    (condition-case nil (imenu-add-to-menubar "Imenu")
      (error nil))))

(use-package imenu-list
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left
        imenu-list-size 40))

(use-package dired
  :straight (dired :type built-in)
  :general
  (pdcmacs-app-def "d" 'dired)
  (pdcmacs-jump-def
    "d" 'dired-jump
    "D" 'dired-jump-other-window)
  (:keymaps 'dired-mode-map
    ", w" 'wdired-change-to-wdired-mode)

  :init
  (setopt dired-use-ls-dired nil
          dired-omit-file-p t
          dired-omit-files "^\\.?#"
          dired-dwim-target t))

(use-package dired-x
  :straight (dired-x :type built-in)
  :commands (dired-jump dired-jump-other-window dired-omit-mode))

(use-package recentf
  :straight (recentf :type built-in)
  :hook
  (after-init . recentf-mode)
  (find-file . pdc/recentf-find-file-hook)
  :init
  (defun pdc/recentf-find-file-hook ()
    (unless recentf-mode
      (recentf-mode)
      (recentf-track-opened-file)))

  (setopt recentf-max-saved-items 1000
          recentf-auto-cleanup 'never
          recentf-auto-save-teimer (run-with-idle-timer 600 t 'recentf-save-list))
  :config
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package ediff
  :straight (ediff :type built-in)
  :init
  (setopt ediff-window-setup-function 'ediff-setup-windows-plain)
  (require 'outline)
  :hook
  (ediff-prepare-buffer . 'show-all)
  (ediff-quit . 'winner-undo))

(use-package flycheck
  :commands global-flycheck-mode
  :diminish " ⓢ"
  :init
  (global-flycheck-mode t))

;;; Setup common lisp mode stuff

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     extempore-mode)
  "A list of Lisp style modes")

(defvar lisp-mode-hooks
  (--map (intern (concat (symbol-name it) "-hook"))
         lisp-modes)
  "Hook variables associated with `lisp-modes'.")

;; (use-package eldoc
;;   :straight (eldoc :type built-in)
;;   :diminish eldoc-mode
;;   :hook
;;   ((eval-expression-minibuffer-setup ielm-mode) . 'eldoc-mode))

(use-package elisp-mode
  :straight (elisp-mode :type built-in)
  :general
  (pdcmacs-mode :keymaps 'emacs-lisp-mode-map
    "c" 'finder-commentary
    "f" 'find-function
    "F" 'find-face-definition)
  :init
  (defun pdc/elisp-mode-hook ()
    (eldoc-mode 1)
    (setq mode-name "EL"))
  :hook
  (emacs-lisp-mode . pdc/elisp-mode-hook))

(use-package eros
  :init
  (eros-mode 1))

(use-package calendar
  :custom
  (calendar-date-style 'iso))

(use-package editorconfig
  :diminish
  :config
  (editorconfig-mode 1))

(use-package pdf-tools
  :if (display-graphic-p))

(use-package notifications
  :straight (notifications :type built-in))


(setq font-lock-mode-hook (cdr font-lock-mode-hook))

(desktop-save-mode 1)
(save-place-mode 1)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'pdf-view-mode-hook 'auto-revert-mode)


(require 'pdcmacs-feeds)
(require 'pdcmacs-org)
(require 'pdcmacs-hugo-support)
(require 'pdcmacs-webservice)
