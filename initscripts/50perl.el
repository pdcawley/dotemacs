;; Use cperl mode instead of the default perl mode
(use-package cperl-mode
  :load-path "cperl-mode"
  :init
  (progn
    (defalias 'perl-mode 'cperl-mode)
    (add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))

    (defun pdc:path->perl-module (path)
      (if (string-match "\\(?:/gui/[^/]+/\\|/lib/\\(?:perl/\\)?\\)\\(.*\\)\\.pm" path)
          (let ((module (match-string 1 path)))
            (while (string-match "/" module)
              (setq module (replace-match "::" nil nil module)))
            module)
        nil))

    ;; Load an application template in a new unattached buffer...

    (defun application-template-pl (  )
      "Inserts the standard Perl application template"
                                        ; For help and info.

      (interactive "*")
                                        ; Make this user accessible.

      (switch-to-buffer "application-template-pl")
      (insert-file "~/.code_templates/perl_application.pl"))

    ;; Load a module template in a new unattached buffer...

    (defun module-template-pm (  )
      "Inserts the standard Perl module template"
      (interactive "*")
      (switch-to-buffer "module-template-pm")
      (insert-file "~/.code_templates/perl_module.pm"))

    (defun cperl-backward-to-start-of-continued-exp (lim)
      (goto-char containing-sexp)
      (let ((sexp-start (following-char)))
        (forward-char)
        (skip-chars-forward " \t\n")
        (if (memq sexp-start (append "([" nil))
            (backward-char cperl-continued-statement-offset))))

    (defun pdc/indent-cperl-indentable (i parse-data)
      (cond            ;;; [indentable terminator start-pos is-block]
       ((eq 'terminator (elt i 1)) ; Lone terminator of "indentable string"
        (goto-char (elt i 3))   ; prev line
        (current-indentation))
       ((eq 'first-line (elt i 1)); [indentable first-line start-pos]
        (goto-char (elt i 2))
        (+ cperl-continued-statement-offset
           (current-indentation)))
       ((eq 'cont-line (elt i 1)); [indentable cont-line pos prev-pos first-char start-pos]
        ;; Indent as the level after closing parens
        (goto-char (elt i 2))   ; indent line
        (skip-chars-forward " \t)") ; Skip closing parens
        (setq p (point))
        (goto-char (elt i 3))   ; previous line
        (skip-chars-forward " \t)") ; Skip closing parens
        ;; Number of parens in between:
        (setq p (nth 0 (parse-partial-sexp (point) p))
              what (elt i 4))   ; First char on current line
        (goto-char (elt i 3))   ; previous line
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
        (error "Unrecognized value of indent: %s" i))))

    (push '(indentable pdc/indent-cperl-indentable)
          cperl-indent-rules-alist)))
