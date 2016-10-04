;; 47ruby.el --- Custom ruby-mode configuration

;; Set those modes!
(req-package ruby-mode
  :requires align
  :mode ("ruby" . ruby-mode)
  :bind
  (:map ruby-mode-map
        ("C-m" . reindent-then-newline-and-indent)
        ;("C-m C-a" . ruby-annotate-buffer)
   :map help-map
        ("r" . ri))
  :config

  ;; Like c-in-literal, only for Ruby
  (defun ruby-in-literal ()
    (let* ((here (point))
           (state (save-excursion
                    (ruby-beginning-of-defun)
                    (parse-partial-sexp (point) here))))
      (or (nth 3 state)
          (nth 4 state)
          nil)))

  ;; Pipe the current buffer through mfp's xmpfilter
  (defun ruby-annotate-buffer ()
    "Send the current current buffer to the annotation filter."
    (interactive "*")
    (let ((initial-line (count-lines (point-min) (point)))
          (initial-char (- (point) (point-at-bol))))
      (shell-command-on-region (point-min) (point-max) "xmpfilter.rb -a" nil t)
      (goto-line initial-line)
      (forward-char initial-char)))

  ;; Redefine this ruby-electric function so that we can use
  ;; ruby-electric-space w/o the minor mode (which doesn't play nice w/
  ;; multiple major modes).
  (defun ruby-electric-code-at-point-p()
    (let* ((properties (text-properties-at (point))))
      (and (null (memq 'font-lock-string-face properties))
           (null (memq 'font-lock-comment-face properties)))))

  ;; Add the Unit::Test output to the list of regexps understood by the
  ;; compile buffer
  (add-to-list 'compilation-error-regexp-alist
               '("\\(\\./[^:]*\\):\\([0-9]*\\)" 1 2))
  ;; (REGEXP FILE-IDX LINE-IDX
                                        ;(setq compilation-error-regexp-alist (cdr compilation-error-regexp-alist))

  ;; Alignment
  (add-to-list 'align-rules-list
               '(ruby-comma-delimiter
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-literal
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))



  ;; RI everywhere!
  (define-key help-map "r" 'ri))