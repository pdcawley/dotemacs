;;; 44elisp.el --- Custom emacs-lisp-mode configuration
(eval-when-compile
  (require 'pdc-utils)
  (require 'cl))

(defvar dss-lisp-modes-hook nil)

(defun dss/init ()
  (linum-mode t)
  (paredit-mode +1)
  (run-hooks dss-lisp-modes-hook))

(defun dss/test-mini-buffer (string)
  (interactive "sWhat: ")
  (message string))


(use-package yasnippet)

(use-package lisp-mode
  :bind (("C-h e c" . finder-commentary)
         ("C-h e e" . view-echo-area-messages)
         ("C-h e f" . find-function)
         ("C-h e F" . find-face-definition))
  :init
  (progn
    (defvar lisp-find-map)
    (define-prefix-command 'lisp-find-map)

    (bind-key "C-h e" 'lisp-find-map)

    (mapc (lambda (major-mode)
            (font-lock-add-keywords
             major-mode
             '(("(\\(lambda\\)\\>"
                (0 (ignore
                    (compose-region (match-beginning 1)
                                    (match-end 1) ?λ))))
               ("(\\(ert-deftest\\)\\>[     '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
                (1 font-lock-keyword-face)
                (2 font-lock-function-name-face
                   nil t)))))
          lisp-modes)

    (defvar slime-mode nil)
    (defvar lisp-mode-initialized nil)

    (defun initialize-lisp-mode ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t)

        (use-package redshank
          :diminish redshank-mode)

        (use-package elisp-slime-nav
          :diminish elisp-slime-nav-mode)

        (use-package edebug)

        (use-package eldoc
          :diminish eldoc-mode
          :defer t
          :init
          (use-package eldoc-extension
            :disabled t
            :defer t
            :init
            (add-hook 'emacs-lisp-mode-hook
                      #'(lambda () (require 'eldoc-extension)) t))

          :config
          (eldoc-add-command 'paredit-backward-delete
                             'paredit-close-round))

        ;; (use-package cldoc
        ;;   :diminish cldoc-mode)

        (use-package ert
          :commands ert-run-tests-interactively
          :bind ("C-c e t" . ert-run-tests-interactively))

        (use-package elint
          :commands 'elint-initialize
          :init
          (defun elint-current-buffer ()
            (interactive)
            (elint-initialize)
            (elint-current-buffer))

          :config
          (progn
            (add-to-list 'elint-standard-variables 'current-prefix-arg)
            (add-to-list 'elint-standard-variables 'command-line-args-left)
            (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
            (add-to-list 'elint-standard-variables 'emacs-major-version)
            (add-to-list 'elint-standard-variables 'window-system)))

        (use-package highlight-cl
          :ensure t
          :init
          (mapc (function
                 (lambda (mode-hook)
                   (add-hook mode-hook
                             'highlight-cl-add-font-lock-keywords)))
                lisp-mode-hooks))

        (defun my-byte-recompile-file ()
          (save-excursion
            (byte-recompile-file buffer-file-name)))

        (use-package info-lookmore
          :init
          (progn
            (info-lookmore-elisp-cl)
            (info-lookmore-elisp-userlast)
            (info-lookmore-elisp-gnus)
            (info-lookmore-apropos-elisp)))

        (defun emacs-lisp-rebuild-associated-elc ()
          "If you're saving an elisp file, likely the .elc is no longer valid"
          (make-local-variable 'after-save-hook)
          (add-hook 'after-save-hook 'my-byte-recompile-file))

        (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-rebuild-associated-elc)

        (mapc #'(lambda (mode)
                  (info-lookup-add-help
                   :mode mode
                   :regexp "[^][()'\" \t\n]+"
                   :ignore-case t
                   :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
              lisp-modes)))

    (defun dss/goto-match-paren (arg)
      "Go to the matching parenthesis if on parenthesis. Else go to the
      opening parenthesis one level up.

      Source: http://www.emacswiki.org/emacs/ParenthesisMatching via Tavis
      Rudd."
      (interactive "p")
      (cond ((looking-at "\\s(") (forward-list 1))
            (t
             (backward-char 1)
             (cond ((looking-at "\\s\)")
                    (forward-char 1) (backward-list 1))
                   (t
                    (while (not (looking-at "\\s\)"))
                      (backward-char 1)
                      (cond ((looking-at "\\s\)")
                             (forward-char 1)
                             (backward-list 1)
                             (backward-char 1)))))))))

    (defun dss/replace-sexp ()
      (interactive)
      (if (dss/in-string-p)
          (dss/mark-string)
        (mark-sexp))
      (delete-region (point) (mark))
      (yank))

    (defun my-lisp-mode-hook ()
      (initialize-lisp-mode)

      (auto-fill-mode 1)
      (paredit-mode +1)
      (redshank-mode 1)
      (elisp-slime-nav-mode 1)

      (local-set-key (kbd "<return>") 'paredit-newline)

      (add-hook 'after-save-hook 'check-parens nil t)

      (if (memq major-mode
                '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
          (progn
            (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map))
        ;; (turn-on-cldoc-mode)

        (bind-key "M-q" 'slime-reindent-defun lisp-mode-map)
        (bind-key "M-l" 'slime-selector lisp-mode-map))

      (yas-minor-mode 1))

    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)

    (defun pdc/elisp-mode-hook ()
      (eldoc-mode 1)
      (setq mode-name "EL: ")
      (setq hippie-expand-try-functions-list
            '(try-expand-dabbrev-visible
              try-complete-lisp-symbol
              try-complete-lisp-symbol-partially
              try-expand-dabbrev)))
    (add-hook 'emacs-lisp-mode-hook 'pdc/elisp-mode-hook ())))

(use-package nukneval)

(use-package ielm
  :bind ("C-c :" . ielm)
  :config
  (progn
    (defun my-ielm-return ()
      (interactive)
      (let ((end-of-sexp (save-excursion
                           (goto-char (point-max))
                           (skip-chars-backward " \t\n\r")
                           (point))))
        (if (>= (point) end-of-sexp)
            (progn
              (goto-char (point-max))
              (skip-chars-backward " \t\n\r")
              (delete-region (point) (point-max))
              (call-interactively #'ielm-return))
          (call-interactively #'paredit-newline))))

    (defun ielm-auto-complete ()
      "Enables `auto-complete' support in \\[ielm]."
      (setq ac-sources '(ac-source-functions
                         ac-source-variables
                         ac-source-features
                         ac-source-symbols
                         ac-source-words-in-same-mode-buffers))
      (add-to-list 'ac-modes 'inferior-emacs-lisp-mode)
      (auto-complete-mode 1))

    (defun dss/ielm-mode-hook ()
      (interactive)
      (ielm-auto-complete)
      (paredit-mode t)
      (bind-key "<return>" 'my-ielm-return ielm-map))

    (defun dss/ielm-set-working-buffer (buf)
      (interactive "bBuffer:")
      (ielm-change-working-buffer buf)
      (setq header-line-format (list (buffer-name ielm-working-buffer))))

    (defun dss/ielm-on-buffer ()
      (interactive)
      (let ((buf (current-buffer)))
        (ielm)
        (dss/ielm-set-working-buffer buf)))

    (add-hook 'ielm-mode-hook 'dss/ielm-mode-hook)))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'scheme-mode
                        '(("(\\|)" . 'paren-face)))

(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey20"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'pdc-faces)

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (paredit-mode))))

(defun dss/eval-expression ()
  (interactive)
  (let ((dss-minibuffer-truncate-lines nil)
        (resize-mini-windows t))
    (call-interactively 'eval-expression)))

(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (when (eq this-command 'dss/eval-expression)
    (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)



(use-package paredit
  :commands paredit-mode
  :diminish "PE"
  :config
  (progn
    ;; (use-package paredit-ext)

    (defun dss/paredit-backward-delete ()
      (interactive)
      (if mark-active
          (call-interactively 'delete-region)
        (paredit-backward-delete)))

    (defun pdc/indent-defun-or-region ()
      (interactive)
      (if mark-active
          (call-interactively 'indent-region)
        (dss/indent-defun)))

    (bind-key "DEL" 'dss/paredit-backward-delete paredit-mode-map)
    (bind-key "M-RET" 'dss/indent-defun paredit-mode-map)

    (bind-key "C-M-l" 'paredit-recentre-on-sexp paredit-mode-map)
    (bind-key "C-M-s" 'paredit-backward-up paredit-mode-map)
    (bind-key "C-M-k" 'paredit-forward-slurp-sexp paredit-mode-map)
    (bind-key "C-M-j" 'paredit-backward-slurp-sexp paredit-mode-map)
    (bind-key "C-M-\\"
              (lambda ()
                (interactive)
                (pdc/indent-defun-or-region)
                (back-to-indentation))
              paredit-mode-map)

    (defun dss/paredit-yank ()
      (interactive)
      (call-interactively 'yank)
      (unless mark-active
        (when (and (looking-back "\)")
                   (looking-at "\("))
          (reindent-then-newline-and-indent)
          (if (looking-at-p "^")
              (newline))))
      (condition-case nil (dss/indent-defun)))

    (bind-key "C-y" 'dss/paredit-yank paredit-mode-map)

    (defun dss/paredit-open-parenthesis ()
      (interactive
       (cond ((and (looking-back "\(")
                   (looking-at "\)"))
              (paredit-open-parenthesis))
             ((equal last-command this-command)
              (undo)
              (insert " ")
              (backward-char 1)
              (paredit-open-parenthesis))
             ((and (not (or mark-active (dss/in-string-p)))
                   (looking-at-p "[\(a-z\"#\\[{]"))
              (mark-sexp)
              (paredit-open-parenthesis)
              (if (looking-at-p "[\(\"#\\[{]")
                  (save-excursion (insert " "))))
             (t (paredit-open-parenthesis)))))

    (bind-key "(" 'dss/paredit-open-parenthesis paredit-mode-map)

    (defun dss/paredit-semicolon ()
      (interactive)
      (when (looking-at-p "  +\(")
        (search-forward "(")
        (backward-char))
      (cond ((or (looking-back ";")
                 (looking-at-p "[[:blank:]]*$"))
             (self-insert-command 1))
            ((equal last-command this-command)
             (undo)
             (self-insert-command 1))
            ((and (not mark-active) (looking-at-p "\("))
             (mark-sexp)
             (paredit-comment-dwim)
             (indent-according-to-mode))
            ((and (not mark-active)
                  (looking-at-p "^[[:blank:]]*$"))
             (insert ";;; "))
            ((and (not mark-active)
                  (looking-back "^[[:blank:]]*")
                  (looking-at-p "[[:blank:]]*$"))
             (insert ";; "))
            (t (paredit-semicolon))))
    (bind-key ";" 'dss/paredit-semicolon paredit-mode-map)

    (defun dss/paredit-open-line ()
      (interactive)
      (save-excursion
        (reindent-then-newline-and-indent))
      (indent-according-to-mode))
    (bind-key "M-o" 'dss/paredit-open-line paredit-mode-map)

    (bind-key "C-M-y" 'dss/replace-sexp paredit-mode-map)
    (bind-key "C-M-y" 'dss/replace-sexp)

    (defun dss/paredit-kill-ring-save ()
      (interactive)
      (if (not mark-active)
        (save-excursion
          (when (looking-at-p " +\(")
            (search-forward "(")
            (backward-char))
          (mark-sexp)
          (call-interactively 'kill-ring-save))
        (call-interactively 'kill-ring-save)))
    (bind-key "M-w" 'dss/paredit-kill-ring-save paredit-mode-map)

    (bind-key ")" 'paredit-close-round-and-newline paredit-mode-map)
    (bind-key "M-)" 'paredit-close-round paredit-mode-map)

    (bind-key "M-k" 'paredit-raise-sexp paredit-mode-map)
    ;; (bind-key "M-h" 'mark-containing-sexp paredit-mode-map)
    (bind-key "M-I" 'paredit-splice-sexp paredit-mode-map)

    (unbind-key "M-r" paredit-mode-map)
    (unbind-key "M-s" paredit-mode-map)

    (bind-key "C-. d" 'paredit-forward-down paredit-mode-map)
    (bind-key "C-. B" 'paredit-splice-sexp-killing-backward paredit-mode-map)
    (bind-key "C-. C" 'paredit-convolute-sexp paredit-mode-map)
    (bind-key "C-. F" 'paredit-splice-sexp-killing-forward paredit-mode-map)
    (bind-key "C-. a" 'paredit-add-to-next-list paredit-mode-map)
    (bind-key "C-. A" 'paredit-add-to-previous-list paredit-mode-map)
    (bind-key "C-. j" 'paredit-join-with-next-list paredit-mode-map)
    (bind-key "C-. J" 'paredit-join-with-previous-list paredit-mode-map)

    (defun dss/in-slime-repl-p ()
      (equal mode-name "REPL"))

    (add-hook 'allout-mode-hook
              #'(lambda ()
                  (bind-key "M-k" 'paredit-raise-sexp allout-mode-map)
                  ;; (bind-key "M-h" 'mark-containing-sexp allout-mode-map)
                  ))))

(defun dss/find-function-at-point ()
  "Find directly the fuction at point in the current window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))

(bind-key "M-." 'dss/find/function-at-point emacs-lisp-mode-map)
