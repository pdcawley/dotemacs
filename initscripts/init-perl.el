;;; lexical-binding: t
(req-package mode-compile
  :loader :path)
(req-package cperl-mode
  :require s rx mode-compile
  :loader :path
  :mode
  (("\\.t\\'"     . perl-mode)
   ("cpanfile\\'" . perl-mode))
  :init
  (progn
    (setq cperl-sub-keywords (list "sub" "method" "class" "role" "fun")
          cperl-sub-regexp (regexp-opt cperl-sub-keywords)
          cperl-qualified-name-rex
          "\\(::[a-zA-Z_0-9:']+\\|[a-zA-Z_'][a-zA-Z_0-9:']*\\)")

    (defun pdc:path->perl-module (path)
      (if (string-match "\\(?:/gui/[^/]+/\\|/lib/\\(?:perl/\\)?\\)\\(.*\\)\\.pm" path)
          (s-replace "/" "::" (match-string 1 path))
        nil))

;;; CPD refactoring
    (defun cpd-related-driver (path)
      (let ((parts (f-split path)))
        ))


    (defun pdc/package-type ()
      "Determines the perl package type.
Returns one of the following symbols `moose-role' `moose-class' `module'
`class' `role'"
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (cond
         ((re-search-forward "use +Moops;" (point-max) t)
          (if (re-search-forward "^\\(class\\|role\\) " (point-max) t)
              (intern (match-string 1))
            'moops))
         ((re-search-forward "use +Moose::Role;" (point-max) t)
          'moose-role)
         ((re-search-forward "use +Moose;" (point-max) t)
          'moose-class)
         (t 'module)))))
  :config
  (progn
    (push '("ThermeonOld"
            (cperl-auto-newline)
            (cperl-brace-imaginary-offset . 4)
            (cperl-brace-offset . 4)
            (cperl-close-paren-offset . 0)
            (cperl-continued-statement-offset . 4)
            (cperl-fix-hanging-brace-when-indent)
            (cperl-indent-level . 0)
            (cperl-indent-parens-as-block . t)
            (cperl-label-offset . -4)
            (cperl-merge-trailing-else)
            (cperl-tab-always-indent . t))
          cperl-style-alist)
    (push '("ThermeonNew"
            (cperl-auto-newline)
            (cperl-brace-imaginary-offset . 0)
            (cperl-brace-offset . 0)
            (cperl-close-paren-offset . -4)
            (cperl-continued-statement-offset . 4)
            (cperl-fix-hanging-brace-when-indent)
            (cperl-indent-level . 4)
            (cperl-indent-parens-as-block  . t)
            (cperl-indent-subs-specially)
            (cperl-label-offset . -4)
            (cperl-merge-trailing-else)
            (cperl-tab-always-indent . t)
            )
          cperl-style-alist)

    (defun pdc/cperl-indent-or-complete ()
      (interactive)
      (when (zerop (call-interactively 'cperl-indent-command))
        (call-interactively 'auto-complete)))

    (defun pdc/turn-on-perl-mode-bindings ()
      (bind-key "C-c P" 'insert-counting-printf cperl-mode-map)
      (bind-key "<M-tab>" 'ac-complete cperl-mode-map)
      (bind-key "<return>" 'newline-and-indent cperl-mode-map)
      (bind-key ":" 'self-insert-command cperl-mode-map))

    (defun pdc/cperl-mode-hook ()
      (abbrev-mode 1)
      (which-function-mode 1)
      (auto-complete-mode 1)
      (yas-minor-mode-on)
      (bug-reference-prog-mode 1)
      (setq ac-sources '(ac-source-abbrev
                         ac-source-imenu
                         ac-source-words-in-same-mode-buffers
                         ac-source-yasnippet))
      (pdc/turn-on-perl-mode-bindings))

    (defun rx-perl-tokens (&rest sexps)
      (let ((WS `(regexp ,cperl-white-and-comment-rex)))
        `(: ,WS ,@(-interpose WS (--map `(regexp ,it)
                                        sexps)))))

    (let ((rx-constituents
           (cons `(WS . ,cperl-white-and-comment-rex) rx-constituents)))
      (setq cperl-after-moops-method
            (rx-to-string
             `(0+ WS
                  (| (: "using" WS (| "Moops" "Moose"))
                     (: (| "with" "extends")
                        ,(rx-perl-tokens cperl-qualified-name-rex))
                     (: "types" ,(rx-perl-tokens cperl-qualified-name-rex)
                        (0+ ,(rx-perl-tokens cperl-qualified-name-rex)))))))

      (setq cperl-imenu--function-name-regexp-perl
            (rx-to-string
             `(: bol
                 (group
                  (| (group (0+ blank) "package"
                            (group ,(rx-perl-tokens
                            cperl-qualified-name-rex)))
                     (group (0+ blank)
                            (: (regexp ,cperl-sub-regexp)
                               (regexp ,(cperl-after-sub-regexp 'named nil))
                               (regexp ,cperl-after-moops-method)
                               (regexp ,cperl-maybe-white-and-comment-rex)))
                     (: "=head"
                        (group (any "1-4"))
                        (0+ blank)
                        (group (0+ (not (any "\n"))))
                        eol))))))

      (setq cperl-outline-regexp
            (rx-to-string `(| (regexp ,cperl-imenu--function-name-regexp-perl)
                              string-start)))

      (setq pdc/cperl-defun-prompt-regexp
            (rx-to-string
             `(: (regexp "^[ \t]*")
                 (submatch
                  (| (: (regexp ,cperl-sub-regexp)
                        (regexp ,(cperl-after-sub-regexp 'named 'attr-groups))
                        (regexp ,cperl-after-moops-method))
                     (| "BEGIN" "UNITCHECK" "CHECK" "INIT" "END" "AUTOLOAD"
                        "DESTROY"))
                  (regexp ,cperl-maybe-white-and-comment-rex))))))

    (defun pdc/fix-cperl-defun-match ()
      "Fixup defun-prompt-regexp to play nice with Moops"
      (setq defun-prompt-regexp pdc/cperl-defun-prompt-regexp))

    (add-hook 'cperl-mode-hook 'pdc/cperl-mode-hook)
    (add-hook 'cperl-mode-hook 'pdc/fix-cperl-defun-match)

    (defalias 'perl-mode 'cperl-mode)

    (defun cperl-backward-to-start-of-continued-exp (lim)
      (goto-char containing-sexp)
      (let ((sexp-start (following-char)))
        (forward-char)
        (skip-chars-forward " \t\n")
        (if (memq sexp-start (append "([" nil))
            (backward-char cperl-continued-statement-offset))))

    (defun pdc/indent-cperl-indentable (i parse-data)
      (cond ;;; [indentable terminator start-pos is-block]
       ((eq 'terminator (elt i 1))    ; Lone terminator of "indentable string"
        (goto-char (elt i 3))         ; prev line
        (current-indentation))
       ((eq 'first-line (elt i 1))      ; [indentable first-line start-pos]
        (goto-char (elt i 2))
        (+ cperl-continued-statement-offset
           (current-indentation)))
       ((eq 'cont-line (elt i 1)) ; [indentable cont-line pos prev-pos first-char start-pos]
        ;; Indent as the level after closing parens
        (goto-char (elt i 2))           ; indent line
        (skip-chars-forward " \t)")     ; Skip closing parens
        (setq p (point))
        (goto-char (elt i 3))           ; previous line
        (skip-chars-forward " \t)")     ; Skip closing parens
        ;; Number of parens in between:
        (setq p (nth 0 (parse-partial-sexp (point) p))
              what (elt i 4))           ; First char on current line
        (goto-char (elt i 3))           ; previous line
        (+ (* p (or cperl-regexp-indent-step cperl-indent-level))
           (cond ((eq what ?\) )
                  (- cperl-close-paren-offset)) ; compensate
                 ((eq what ?\| )
                  (- (or cperl-regexp-indent-step cperl-indent-level)))
                 (t 0))
           (if (eq (following-char) ?\| )
               (or cperl-regexp-indent-step cperl-indent-level)
             0)
           (current-column)))
       (t
        (error "Unrecognized value of indent: %s" i)))

      (pushnew '(indentable pdc/indent-cperl-indentable)
               cperl-indent-rules-alist))))