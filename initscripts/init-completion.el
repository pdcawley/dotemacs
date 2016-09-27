(require 'comint)
(require 'dss-paths)
(eval-when-compile (require 'cl))


(req-package abbrev
  :diminish " ⓐ"
  :init
  (progn
    (setq-default abbrev-mode t)
    (setq save-abbrevs t)
    (if (file-exists-p abbrev-file-name)
        (read-abbrev-file abbrev-file-name t)))


  (setq-default dabbrev-case-replace nil))
(setq-default hippie-expand-try-functions-list
              '(try-expand-abbrev
                try-expand-dabbrev-visible
                try-expand-dabbrev))

(req-package auto-complete)

(req-package auto-complete-config
  :requires auto-complete
  :diminish (auto-complete-mode " Ⓐ")
  :init
  (progn
    (add-to-list 'ac-dictionary-directories (concat dss-dotfiles-dir
    "ac-dict"))
    (ac-config-default)
    (setq ac-delay 0.2
          ac-auto-show-menu 0.3
          ac-menu-height 20
          ac-use-comphist nil
          ac-candidate-limit 25
          ac-use-quick-help nil))
  :config
  (progn
    (defun dss/ac-hide-help ()
      (interactive)
      (setq ac-use-quick-help nil))

    (defun dss/ac-show-help ()
      (interactive)
      (setq ac-use-quick-help t))

    (defun ac-complete-no-fallback ()
      "a hacked version of ac-complete with fallback disabled.
  For some reason this is very sensitive to the function name
  prefix 'ac-'."
      (interactive)
      (let* ((candidate (ac-selected-candidate))
             (action (popup-item-property candidate 'action))
             (fallback nil))
        (when candidate
          (unless (ac-expand-string candidate)
            (setq fallback nil)) ;
          ;; Remember to show help later
          (when (and ac-point candidate)
            (unless ac-last-completion
              (setq ac-last-completion (cons (make-marker) nil)))
            (set-marker (car ac-last-completion) ac-point ac-buffer)
            (setcdr ac-last-completion candidate)))
        (ac-abort)
        (cond
         (action
          (funcall action))
         (fallback
          (ac-fallback-command)))
        candidate))

    (defun dss/ac-electric-pair ()
      (interactive)
      (ac-complete-no-fallback)
      (dss/electric-pair))

    (defun dss/ac-electric-char ()
      (interactive)
      (ac-complete-no-fallback)
      (call-interactively 'self-insert-command))

    ;; (define-key ac-completing-map "(" 'dss/ac-electric-pair)
    ;; (define-key ac-completing-map "[" 'dss/ac-electric-pair)
    ;; (define-key ac-completing-map "," 'dss/ac-electric-char)
    ;; (define-key ac-completing-map " " 'dss/ac-electric-char)
    ;; (define-key ac-completing-map "\r" 'ac-complete-no-fallback)
    ;; (define-key ac-completing-map (kbd "M-/") 'dabbrev-completion)
    ;; (define-key ac-completing-map (kbd "M-/") 'dss/hippie-expand)
    (define-key ac-completing-map (kbd "M-/") 'ac-expand)

;;; <up/down> in terminal
    (define-key ac-completing-map "\e[A" 'ac-previous)
    (define-key ac-completing-map "\e[B" 'ac-next)

;;; C-<up/down> in terminal
    (define-key ac-completing-map "\eOa" 'ac-quick-help-scroll-up)
    (define-key ac-completing-map "\eOb" 'ac-quick-help-scroll-down)

    (defvar ac-org-candidates nil)
    (defvar ac-org-pattern nil)
    (defun ac-org-construct-candidates ()
      "Pabbrev source for org."
      (let* ((end (point))
             (beg1 (save-excursion
                     (skip-chars-backward (org-re "[:alnum:]_@"))
                     (point)))
             (beg (save-excursion
                    (skip-chars-backward "a-zA-Z0-9_:$")
                    (point)))
             (confirm (lambda (x) (stringp (car x))))
             (searchhead (equal (char-before beg) ?*))
             (struct
              (when (and (member (char-before beg1) '(?. ?<))
                         (setq a (assoc (buffer-substring beg1 (point))
                                        org-structure-template-alist)))
                (org-complete-expand-structure-template (1- beg1) a)
                (throw 'exit t)))
             (tag (and (equal (char-before beg1) ?:)
                       (equal (char-after (point-at-bol)) ?*)))
             (prop (and (equal (char-before beg1) ?:)
                        (not (equal (char-after (point-at-bol)) ?*))))
             (texp (equal (char-before beg) ?\\))
             (link (equal (char-before beg) ?\[))
             (opt (equal (buffer-substring (max (point-at-bol) (- beg 2))
                                           beg)
                         "#+"))
             (startup (string-match "^#\\+STARTUP:.*"
                                    (buffer-substring (point-at-bol) (point))))
             (completion-ignore-case opt)
             (type nil)
             (tbl nil)
             (table (cond
                     (opt
                      (setq type :opt)
                      (require 'org-exp)
                      (append
                       (mapcar
                        (lambda (x)
                          (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                          (cons (match-string 2 x) (match-string 1 x)))
                        (org-split-string (org-get-current-options) "\n"))
                       (mapcar 'list org-additional-option-like-keywords)))
                     (startup
                      (setq type :startup)
                      org-startup-options)
                     (link (append org-link-abbrev-alist-local
                                   org-link-abbrev-alist))
                     (texp
                      (setq type :tex)
                      org-html-entities)
                     ((string-match "\\`\\*+[ \t]+\\'"
                                    (buffer-substring (point-at-bol) beg))
                      (setq type :todo)
                      (mapcar 'list org-todo-keywords-1))
                     (searchhead
                      (setq type :searchhead)
                      (save-excursion
                        (goto-char (point-min))
                        (while (re-search-forward org-todo-line-regexp nil t)
                          (push (list
                                 (org-make-org-heading-search-string
                                  (match-string 3) t))
                                tbl)))
                      tbl)
                     (tag (setq type :tag beg beg1)
                          (or org-tag-alist (org-get-buffer-tags)))
                     (prop (setq type :prop beg beg1)
                           (mapcar 'list (org-buffer-property-keys nil t t)))
                     (t (progn
                          (call-interactively org-completion-fallback-command)
                          (throw 'exit nil))))))
        (setq ac-org-pattern (buffer-substring-no-properties beg end))
        table))

    (defvar ac-source-org nil)
    (setq ac-source-org
          `((sigil . "o")
            (init . (lambda () (setq ac-org-candidates
                                (condition-case nil
                                    (ac-org-construct-candidates)))))
            (candidates . (lambda ()
                            (all-completions ac-target ac-org-candidates)))))

    (defun ac-emacs-lisp-mode-setup ()
      (setq ac-sources '(ac-source-words-in-buffer ac-source-functions
      ac-source-variables ac-source-symbols ac-source-features
      ac-source-yasnippet)))))

(req-package go-autocomplete
  :require auto-complete-config)


