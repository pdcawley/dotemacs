;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(defun start-server-after-init ()
  (interactive)
  (require 'server)
  (unless (server-running-p)
    (server-start)
    (midnight-mode +1)))

;; Autorevert stuff

(use-feature autorevert
  :custom
  ;; Revert dired and other buffers
  (global-auto-revert-non-file-buffers t)
  :config
  ;; Revert buffers when the underlying file changed.
  (global-auto-revert-mode 1))

;;(use-feature eldoc-mode :diminish)

;; Enable some stuff
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;
;; Editing stuff

;; trim excess whitespace
(use-package ws-butler
  :diminish
  :hook
  ((prog-mode text-mode) . ws-butler-mode))

;; (use-package lispy
;;   :after (paredit embark)
;;   ;; TODO: Work out how to do this with 'bind'
;;   :config
;;   (keymap-unset lispy-mode-map "M-m")
;;   (keymap-unset lispy-mode-map "M-."))

                                        ;
(defun pdc/prioritise-paredit-bindings ()
  (push (assoc 'paredit-mode minor-mode-map-alist)
        minor-mode-overriding-map-alist))

(use-package paredit
  :diminish "Ⓟ "
  :bind
  (:map paredit-mode-map
        ("DEL"   . pdc/paredit-backward-delete)
        ("("     . pdc/paredit-open-parenthesis)
        (")"     . paredit-close-round-and-newline)
        ("M-)"   . paredit-close-round)
        ("C-M-l" . paredit-recenter-on-sexp)
        ("C-M-s" . paredit-backward-up)
        ("M-I"   . paredit-splice-sexp)
        ("]"     . paredit-close-square-and-newline)
        ("}"     . paredit-close-curly-and-newline)
        (";"     . pdc/paredit-semicolon))

  :config
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
                                        ; (paredit-mode . (lambda () (if (fboundp 'lispy-mode) (lispy-mode))))
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
  :bind (("C-a" . mwim-beginning)
         ("C-e" . +mwim-next-ending))
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
  :after consult
  :custom
  (vertico-cycle t)
  (vertico-multiform-commands
   '((consult-recent-file buffer)
     (consult-mode-command buffer)
     (consult-complex-command buffer)
     (embark-bindings buffer)
     (consult-locate buffer)
     (consult-project-buffer buffer)
     (consult-ripgrep buffer)
     (consult-fd buffer)))
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-multiform-mode)
  :bind
  (:map vertico-map
        :prefix "M-,"
        :prefix-map vertico-options-map
        ("r" . vertico-reverse-mode)
        ("g" . vertico-grid-mode))
  (:map vertico-map
        ("M-q"        . vertico-quick-insert)
        ("C-q"        . vertico-quick-exit)
        ("C-k"        . kill-whole-line)
        ("C-u"        . kill-whole-line)
        ("C-o"        . vertico-next-group)
        ("<tab>"      . vertico-insert)
        ("TAB"        . vertico-insert)
        ("M-<return>" . minibuffer-force-complete)))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-light nil)))


(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  :init
  (advice-add #'register-preview :override #'consult-register-window)

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (bind-keys ([remap isearch-forward] . consult-line)
             ([remap Info-search]        . consult-info)
             ([remap imenu]              . consult-imenu)
             ([remap recentf-open-files] . consult-recent-file)

             ("C-x M-:" . consult-complex-command)
             ("C-x b"   . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ("C-x r b" . consult-bookmark)
             ("C-x p b" . consult-project-buffer)
             ("M-#"     . consult-register-load)
             ("M-'"     . consult-register-store)
             ("C-M-#"   . consult-register)
             ("M-y"     . consult-yank-pop)
             :map isearch-mode-map
             ("M-e" . consult-isearch-history)
             ("M-s e" . consult-isearch-history)
             ("M-s l" . consult-line)
             ("M-s L" . consult-line-multi)
             :map minibuffer-local-map
             ("C-s" ("insert-current-symbol" . (lambda ()
                                                 "Insert the current symbol"
                                                 (interactive)
                                                 (insert (save-excursion
                                                           (set-buffer (window-buffer (minibuffer-selected-window)))
                                                           (or (thing-at-point 'symbol t) ""))))))
             ("M-s" . consult-history)
             ("M-r" . consult-history)
             :map search-map
             ("d" . consult-find)
             ("D" . consult-locate)
             ("g" . consult-grep)
             ("G" . consult-git-grep)
             ("r" . consult-ripgrep)
             ("l" . consult-line)
             ("L" . consult-line-multi)
             ("k" . consult-keep-lines)
             ("u" . consult-focus-lines)
             ("e" . consult-isearch-history))
  (consult-customize
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any))
  :demand t)



(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completion-category-defaults nil))

(use-package aggressive-indent
  :init (aggressive-indent-global-mode t))

(use-package xref)

(use-package embark :after xref
  :bind
  (("C-." . embark-act)
   ("M-." . embark-act)
   ("M-," . embark-dwim)
   ("C-;" . embark-dwim)
   (([remap describe-bindings] . embark-bindings)))
  :custom
  (embark-cycle-key "M-.")
  (prefix-help-command #'embark-prefix-help-command))

(require 'rx)

;; (let ((parameters '(window-parameters . ((no-other-window . t)
;;                                          (no-delete-other-windows . t)))))

;;   (add-to-list 'display-buffer-alist
;;                '("\\*compilation\\*" display-buffer-no-window
;;                  (allow-no-window . t)))
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx (| "*info*"
;;                          "NEWS"))
;;                  (display-buffer-below-selected)
;;                  (window-height . fit-window-to-buffer)
;;                  ,parameters))
;;   (add-to-list
;;    'display-buffer-alist
;;    `(,(rx "*"
;;           "helpful " (*? anychar) ":" (* (not ?*))
;;           "*")
;;      display-buffer-in-side-window
;;      (side . bottom) (slot . 1) (window-width . fit-window-to-buffer)
;;      (window-parameters . ((no-other-window . 1)
;;                            (no-delete-other-windows . t)))))
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx "*"
;;                       (| "xref"
;;                          "grep"
;;                          "Occur"
;;                          "Completions")
;;                       "*")
;;                  display-buffer-below-selected
;;                  (preserve-size . (nil . t))
;;                  ,parameters)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package ace-window)
(use-package 0x0)

;;; Appearance

(use-package all-the-icons
  :if (display-graphic-p))

;; (use-package all-the-icons-dired-mode
;;   :after all-the-icons
;;   :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package all-the-icons-completion
;;   :after all-the-icons
;;   :hook
;;   (marginalia-mode . all-the-icons-completion-marginalia-setup))

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
  :hook
  (after-init . auto-compile-on-load-mode)
  (after-init . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

;; Make `describe-*' screens more helpful

(use-package helpful
  :bind
  (([remap describe-command]  . helpful-command)
   ([remap describe-function] . helpful-callable)
   ([remap describe-key]      . helpful-key)
   ([remap describe-symbol]   . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ("C-h C"                   . helpful-command)
   ("C-h F"                   . helpful-function)
   ("C-h K"                   . describe-keymap)
   :map helpful-mode-map
   ([remap revert-buffer]     . helpful-update))
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

(use-feature hl-line-mode
  :hook
  ((occur-mode dired-mode package-menu-mode) . hl-line-mode))

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
  :bind
  (:map emacs-lisp-mode-map
        :prefix "M-m ,"
        :prefix-map leader/mode/elisp-map
        :prefix-docstring "mode(elisp)"
        ("e" . macrostep-expand)))

;;; Repeat mode stuffs

(use-package repeat
  :disabled
  :custom
  (repeat-echo-function #'ignore)
  :config
  (repeat-mode t))

(use-package corfu
  :custom
  ;; Works with `indent-for-tab-command'. Make sure tab doesn't indent when you
  ;; want to perform completion
  (tab-always-indent 'complete)
  (tab-first-completion 'word)

  (completion-cycle-threshold 3)

  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.2)
  (corfu-preview-current nil)
  (corfu-quit-at-boundary 'separator)

  (global-corfu-modes '((not org-mode) prog-mode))

  (corfu-preselect nil)
  :hook
  (eshell-history-mode . +eshell-history-mode-setup-completion)
  (lsp-completion-mode . +lsp-mode-setup-completion)


  :bind
  (:map corfu-map
        ("M-SPC"      . corfu-insert-separator)
        ("RET"        . corfu-insert)
        ("S-<return>" . corfu-insert)
        ("M-m"        . +corfu-move-to-minibuffer)
        ("TAB"        . +pdc/corfu-complete-common-or-next)
        ("<tab>"        . +pdc/corfu-complete-common-or-next))

  :init
  ;; TODO: Write a function to attach to tab that first completes a common prefix and, on second hit, inserts the current selection

  (defun +pdc/corfu-complete-common-or-next ()
    "Complete common prefix or go to next candidate."
    (interactive)
    (if (= corfu--total 1)
        (progn
          (corfu--goto 1)
          (corfu-insert))
      (let* ((input (car corfu--input))
             (str (if (thing-at-point 'filename) (file-name-nondirectory input) input))
             (pt (length str))
             (common (try-completion str corfu--candidates)))
        (if (and (> pt 0)
                 (stringp common)
                 (not (string= str common)))
            (insert (substring common pt))
          (corfu-next)))))

  (defun +pdc/corfu-insert ()
    "Insert current candidate or newline."
    (interactive))

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

  (global-corfu-mode t))

(use-package corfu-terminal
  :if
  (not window-system)
  :init
  (corfu-terminal-mode t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yaml)

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


(use-package consult-yasnippet :after (consult yasnippet)
  :bind
  (("M-g y" . consult-yasnippet)))

;; (use-package yasnippets-orgmode
;;   :after org-mode)
(use-package yasnippets :after yasnippet
  :init
  (setq yas-snippet-dirs (cons (straight--el-get-package-directory 'yasnippets)
                               pdc-snippet-dirs))
  (yas-global-mode t))


(use-package visual-fill-column
  :defer nil
  :hook
  ((text-mode org-mode) . visual-fill-column-mode)
  :custom
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-center-text t))

(for-terminal
  (xterm-mouse-mode 1))

(use-package powerline
  :hook
  (after-init . powerline-default-theme))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Movement/jumping
(defvar pdc-jump-map (make-sparse-keymap))

(use-package avy
  :bind
  (("M-m j j" . avy-goto-char-timer)
   ("M-m j b" . avy-goto-char)
   ("M-m j '" . avy-goto-char-2)
   ("M-m j w" . avy-goto-word-1)))

(use-package imenu
  :bind
  (("M-m j i" . imenu))
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
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'left)
  (imenu-list-size 40))

(use-package multiple-cursors
  :bind
  (:prefix "M-m m"
           :prefix-map pdc-multi-map
           :prefix-docstring "multi"
           ("a" . mc/edit-beginnings-of-lines)
           ("e" . mc/edit-ends-of-lines)
           ("^" . mc/edit-beginnings-of-lines)
           ("$" . mc/edit-ends-of-lines)
           ("m" . mc/edit-lines))
  :config)

(use-feature outline)

(use-feature ediff :after outline
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-prepare-buffer . show-all)
  (ediff-quit           . winner-undo))

;; (use-package flycheck
;;   :commands global-flycheck-mode
;;   :diminish " ⓢ"
;;   :hook
;;   (after-init . global-flycheck-mode))

;;; Setup common lisp mode stuff

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     extempore-mode)
  "A list of Lisp style modes.")

(defvar lisp-mode-hooks
  (--map (intern (concat (symbol-name it) "-hook"))
         lisp-modes)
  "Hook variables associated with `lisp-modes'.")

(use-feature eldoc
  :diminish eldoc-mode
  :hook
  ((eval-expression-minibuffer-setup ielm-mode) . eldoc-mode))

(use-feature elisp-mode
  ;; :bind ( :map emacs-lisp-mode-map
  ;;         ("c" . finder-commentary)
  ;;         ("f" . find-function)
  ;;         ("F" . find-face-definition))
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
  :hook
  (after-init . editorconfig-mode))

(use-package pdf-tools :if (display-graphic-p))

(use-feature notifications)

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  (keymap-global-set "M-m d E" '("envrc" . envrc-command-map)))

(use-package json-ts-mode
  :mode ("\\.noisette\\'" "\\.json\\'"))

(use-package project :after envrc
  :bind (:map project-prefix-map
              ("C" . 'recompile)
              ("s" . 'consult-ripgrep))
  :config (dolist (func '(project-find-file project-find-dir project-eshell))
            (advice-add func :after #'envrc-allow)))

(use-package geiser-guile)

(use-package suggest
  :commands (suggest))

(use-package casual-calc
  :bind (:map calc-mode-map ("C-o" . 'casual-calc-tmenu)))
(use-package casual-dired
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))
(use-package casual-info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))
(setq font-lock-mode-hook (cdr font-lock-mode-hook))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package haskell-mode)

(require 'pdcmacs-feeds)
(require 'pdcmacs-org)
(require 'pdcmacs-hugo-support)
(require 'pdcmacs-webservice)
