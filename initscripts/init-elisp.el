;;; 44elisp.el --- Custom emacs-lisp-mode configuration
(eval-when-compile
  (require 'cl))
(require 'pdc-utils)
(req-package s)
(require 'dash)

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))


(req-package autoinsert)
(req-package saveplace)
(req-package ffap)
(req-package cider)

(defvar dss-lisp-modes-hook nil)

(defun dss/init ()
  (nlinum-relative-on)
  (paredit-mode +1)
  (run-hooks dss-lisp-modes-hook))

(defun dss/test-mini-buffer (string)
  (interactive "sWhat: ")
  (message string))


(req-package lisp-mode
  :require (yasnippet nlinum-relative cider)
  :force t
  :bind (("C-h e" . nil)
         ("C-h e c" . finder-commentary)
         ("C-h e e" . view-echo-area-messages)
         ("C-h e f" . find-function)
         ("C-h e F" . find-face-definition))
  :init
  (progn
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

        (defun emacs-lisp-rebuild-associated-elc ()
          "If you're saving an elisp file, likely the .elc is no longer valid"
          (make-local-variable 'after-save-hook)
          (add-hook 'after-save-hook 'my-byte-recompile-file))

        (defun emacs-lisp-fixup-indent-function ()
          (setq-local lisp-indent-function 'Fuco1/lisp-indent-function))

        (defun emacs-lisp-setup-eval-overlay ()
          (autoload 'cider--make-result-overlay "cider-overlays")

          (defun pdc/eval-overlay (value point)
            (cider--make-result-overlay (format "%S" value)
              :where point
              :duration 'command)
            value)

          (advice-add 'eval-region :around
                      (lambda (f beg end &rest r)
                        (pdc/eval-overlay
                         (apply f beg end r)
                         end)))

          (advice-add 'eval-last-sexp :filter-return
                      (lambda (r)
                        (pdc/eval-overlay r (point))))

          (advice-add 'pp-eval-last-sexp :filter-return
                      (lambda (r)
                        (pdc/eval-overlay r (point))))

          (advice-add 'eval-defun :filter-return
                      (lambda (r)
                        (pdc/eval-overlay
                         r
                         (save-excursion
                           (end-of-defun)
                           (point))))))

        (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-setup-eval-overlay)
        (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-rebuild-associated-elc)
        (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-fixup-indent-function)

        ;; (mapc #'(lambda (mode)
        ;;           (info-lookup-add-help
        ;;            :mode mode
        ;;            :regexp "[^][()'\" \t\n]+"
        ;;            :ignore-case t
        ;;            :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
        ;;       lisp-modes)
    ))

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

      (add-hook 'after-save-hook 'check-parens
                nil t)

      (yas-minor-mode 1))


    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)

    (defun pdc/elisp-mode-hook ()
      (eldoc-mode 1)
      (setq mode-name "EL")
      (setq hippie-expand-try-functions-list
            '(try-expand-dabbrev-visible
              try-complete-lisp-symbol
              try-complete-lisp-symbol-partially
              try-expand-dabbrev)))
    (add-hook 'emacs-lisp-mode-hook 'pdc/elisp-mode-hook ())
    (setq emacs-lisp-mode-hook (cl-remove 'lexbind-mode emacs-lisp-mode-hook))
    ))

(req-package redshank
  :require lisp-mode
  :diminish redshank-mode)

(req-package elisp-slime-nav
  :require lisp-mode
  :diminish elisp-slime-nav-mode)

(req-package edebug
  :require lisp-mode)

(req-package eldoc
  :require lisp-mode
  :diminish eldoc-mode
  :defer t
  :init
  :config
  (eldoc-add-command 'paredit-backward-delete
                     'paredit-close-round))

(req-package eldoc-extension
  :require eldoc
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook
            #'(lambda () (require 'eldoc-extension)) t))

;; (req-package cldoc
;;   :require lisp-mode
;;   :diminish cldoc-mode)

(req-package ert
  :require lisp-mode
  :commands ert-run-tests-interactively
  :bind ("C-c e t" . ert-run-tests-interactively))

(req-package elint
  :require lisp-mode
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

(req-package highlight-cl
  :require lisp-mode
  :init
  (mapc (function
         (lambda (mode-hook)
           (add-hook-exec mode-hook
                     'highlight-cl-add-font-lock-keywords)))
        lisp-mode-hooks))

(defun my-byte-recompile-file ()
  (save-excursion
    (byte-recompile-file buffer-file-name)))

(req-package ielm
  :require lisp-mode
  :bind
  (("C-c :" . ielm)
   :map ielm-mode
   ("<return>" . my-ielm-return))
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
      (paredit-mode t))

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

(req-package eval-expr
  :require paredit
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


(req-package elisp-mode
  :require lisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("<M-return>" . outline-insert-heading)
        ("M-." . dcc/find-function-at-point))
  :config
  (defun dss/find-function-at-point ()
    "Find directly the fuction at point in the current window."
    (interactive)
    (let ((symb (function-called-at-point)))
      (when symb
        (find-function symb)))))



(req-package paredit
  :require lisp-mode
  :commands paredit-mode
  :diminish " Ⓟ"
  :bind
  (:map lisp-mode-map
   ("M-q" . slime-reindent-defun)
   ("M-l" . slime-selector)
   :map paredit-mode-map
   ("DEL" . dss/paredit-backward-delete)
   ("M-RET" . dss/indent-defun)
   ("C-M-l" . paredit-recentre-on-sexp)
   ("C-M-s" . paredit-backward-up)
   ("C-M-k" . paredit-forward-slurp-sexp)
   ("C-M-j" . paredit-backward-slurp-sexp)
   ("C-M-\\" . pdc/indent-then-snap-to-indent)
   ("M-o" . dss/paredit-open-line)
   ("C-M-y" . dss/replace-sexp)
   ("C-M-y" . dss/replace-sexp)
   ("C-y" . dss/paredit-yank)
   ("(" . dss/paredit-open-parenthesis)
   (";" . dss/paredit-semicolon)
   ("M-w" . dss/paredit-kill-ring-save)
   (")" . paredit-close-round-and-newline)
   ("M-)" . paredit-close-round)
   ("M-k" . paredit-raise-sexp)
   ("M-I" . paredit-splice-sexp)
   ("C-. d" . paredit-forward-down)
   ("C-. B" . paredit-splice-sexp-killing-backward)
   ("C-. C" . paredit-convolute-sexp)
   ("C-. F" . paredit-splice-sexp-killing-forward)
   ("C-. a" . paredit-add-to-next-list)
   ("C-. A" . paredit-add-to-previous-list)
   ("C-. j" . paredit-join-with-next-list)
   ("C-. J" . paredit-join-with-previous-list))
  :config
  (progn
    ;; (req-package paredit-ext)

    (bindings|add-toggle paredit
      :status paredit-mode
      :on (enable-paredit-mode)
      :off (disable-paredit-mode)
      :keymaps lisp-modes
      :toggle-keys "tp")



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

    (defun pdc/indent-then-snap-to-indent ()
      (interactive)
      (pdc/indent-defun-or-region)
      (back-to-indentation))

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
    (defun dss/paredit-open-line ()
      (interactive)
      (save-excursion
        (reindent-then-newline-and-indent))
      (indent-according-to-mode))

    (defun dss/paredit-kill-ring-save ()
      (Interactive)
      (if (not mark-active)
          (save-excursion
            (when (looking-at-p " +\(")
              (search-forward "(")
              (backward-char))
            (mark-sexp)
            (call-interactively 'kill-ring-save))
        (call-interactively 'kill-ring-save)))

    (unbind-key "M-r" paredit-mode-map)
    (unbind-key "M-s" paredit-mode-map)

    (defun dss/in-slime-repl-p ()
      (equal mode-name "REPL"))))

(defun Fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(provide 'init-elisp)