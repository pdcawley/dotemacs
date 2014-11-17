(require 'skeleton)
(require 'diminish)
(require 'paredit)
(require 'eldoc)
(diminish 'eldoc-mode "")
(diminish 'paredit-mode "PE")

;; (eldoc-add-command
;;  'paredit-backward-delete
;;  'paredit-close-round)

(eval-when-compile
  (require 'cl))
(require 'dss-codenav-helpers)

(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

(defvar dss-lisp-modes-hook nil)

(defun dss/lisp-modes-init ()
  (linum-mode t)
  (paredit-mode +1)
  (dss/highlight-watchwords)
  (run-hooks 'dss-lisp-modes-hook))

(defun dss/test-mini-buffer (string)
  (interactive "sWhat: ")
  (message string))

(defun ielm-auto-complete ()
  "Enables `auto-complete' suport in \\[ielm]."
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

(add-hook 'ielm-mode-hook 'dss/ielm-mode-hook)

(defun dss/eval-expression ()
  (interactive)
  (let ((dss-minibuffer-truncate-lines nil)
        (resize-mini-windows t))
    (call-interactively 'eval-expression)))

(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'dss/eval-expression)
      (progn
        (paredit-mode 1))))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  idea from esk-paren-face in emacs starter Kit
(defface dss-paren-face
  '((((class color))
     ;; 9e9e9e or a8a8a8 are also good
     (:foreground "#8a8a8a")))
  "Face used to dim parentheses."
  :group 'dss-faces)

(defface dss-end-paren-face
  '((((class color))
     (:foreground "#8a8a8a")))         ;#9e9e9e
  "Face used to dim parentheses."
  :group 'dss-faces)

;; this form also from emacs starter kit
(dolist (x '(scheme emacs-lisp lisp slime-repl lisp-interaction clojure))
  (font-lock-add-keywords
   (intern (concat (symbol-name x) "-mode"))
   '(("(" . 'dss-paren-face)      ;("(\\|)" . 'dss-paren-face)
     (")" . 'dss-end-paren-face)
     ))
  (add-hook
   (intern (concat (symbol-name x) "-mode-hook")) 'dss/lisp-modes-init))

(setq dss-greyscale
      ["#080808"
       "#121212"
       "#1c1c1c"
       "#262626"
       "#303030"
       "#3a3a3a"
       "#444444"
       "#4e4e4e"
       "#585858"
       "#626262"
       "#6c6c6c"
       "#767676"
       "#808080"
       "#8a8a8a"
       "#949494"
       "#9e9e9e"
       "#a8a8a8"
       "#b2b2b2"
       "#bcbcbc"
       "#c6c6c6"
       "#d0d0d0"
       "#dadada"
       "#e4e4e4"
       "#eeeeee"])

(defun dss/greyscale-pos (colour)
  (interactive)
  (or
   (position colour dss-greyscale :test 'string=)
   (1- (length dss-greyscale))))

(defun dss/greyscale-incr (colour &optional i)
  (interactive)
  (let* ((i (or i 1))
         (min-or-max (if (> i 0) 'min 'max))
         (arg2 (if (> i 0) (1- (length dss-greyscale)) 0)))
    (elt dss-greyscale
         (funcall min-or-max (+ (dss/greyscale-pos colour) i) arg2))))

(defun dss/greyscale-decr (colour &optional i)
  (interactive)
  (dss/greyscale-incr colour (or i -1)))

;; (dss/greyscale-incr (elt dss-greyscale 0) 25)
;; (dss/greyscale-incr (elt dss-greyscale (1- (length dss-greyscale))) 4)

                                        ;rainbow-delimiters-depth-1-face

;; (loop for i from 1 to 9
;;       do (message (symbol-name (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face")))))
(require 'rainbow-delimiters)
(setq rainbow-delimiters-max-face-count 8)

(defun dss/lisp-setup-rainbow-delimeters ()
  (let ((dss-rainbow-delim-colors
         [
          "red"
          "brightblue"
          "yellow"
          "purple"
          "cyan"
          "#626262"
          "#6c6c6c"
          "#767676"
          "#808080"
          "#8a8a8a"
          "#949494"
          "#9e9e9e"
          "#a8a8a8"
          "#b2b2b2"
          "#bcbcbc"
          "#c6c6c6"
          "#d0d0d0"
          "#dadada"
          "#e4e4e4"
          "#eeeeee"]))
    (loop for i from 1 to rainbow-delimiters-max-face-count
          do (set-face-foreground
              (intern (concat "rainbow-delimiters-depth-" (number-to-string i) "-face"))
              (elt dss-rainbow-delim-colors (1- i))))))

(dss/lisp-setup-rainbow-delimeters)

(defun dss/lisp-rainbow-flash ()
  (interactive)
  (let* ((depth 1)
         (beg (save-excursion
                (cond
                 ((looking-at "\\s\(") (point))
                 (t
                  (progn
                    (call-interactively 'dss/goto-match-paren)
                    (point))))))
         (end (save-excursion
                (cond
                 ((looking-at "\\s\(") (forward-list 1))
                 (t
                  (call-interactively 'dss/goto-match-paren)
                  (forward-list 1)))
                (point))))
    (save-excursion
      (goto-char beg)
      (while (and (< (point) end)
                  (re-search-forward rainbow-delimiters-delim-regex end t))
        (backward-char) ; re-search-forward places point after delim; go back.
        (unless (rainbow-delimiters-char-ineligible-p (point))
          (let ((delim (char-after (point))))
            (cond ((eq ?\( delim)       ; (
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "paren" depth (point)))
                  ((eq ?\) delim)       ; )
                   (rainbow-delimiters-apply-color "paren" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched paren
                                   (1- depth))))
                  ((eq ?\[ delim)       ; [
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "bracket" depth (point)))
                  ((eq ?\] delim)       ; ]
                   (rainbow-delimiters-apply-color "bracket" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched bracket
                                   (1- depth))))
                  ((eq ?\{ delim)       ; {
                   (setq depth (1+ depth))
                   (rainbow-delimiters-apply-color "brace" depth (point)))
                  ((eq ?\} delim)       ; }
                   (rainbow-delimiters-apply-color "brace" depth (point))
                   (setq depth (or (and (<= depth 0) 0) ; unmatched brace
                                   (1- depth)))))))
        ;; move past delimiter so re-search-forward doesn't pick it up again
        (forward-char)))
    (save-excursion (font-lock-fontify-region beg end))
    (sit-for 3)
    (save-excursion
      (rainbow-delimiters-unpropertize-region beg end)
      (font-lock-fontify-region beg end))))

(defun dss/lisp-set-parens-color (i)
  (interactive "n1-23: ")
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (elt dss-greyscale i))))

(defun dss/lisp-brighten-parens ()
  (interactive)
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (dss/greyscale-incr (face-foreground face) 2))))

(defun dss/lisp-dim-parens ()
  (interactive)
  (dolist (face '(dss-paren-face dss-end-paren-face))
    (set-face-foreground face (dss/greyscale-decr (face-foreground face) -2))))

(defun dss/goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up.

  Copied from: http://www.emacswiki.org/emacs/ParenthesisMatching"
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; additions to standard map (non-magical ones that
;;; won't clash too much)

(defun dss/paredit-backward-delete ()
  (interactive)
  (if mark-active
      (call-interactively 'delete-region)
    (paredit-backward-delete)))

(define-key paredit-mode-map (kbd "DEL") 'dss/paredit-backward-delete)
(define-key paredit-mode-map (kbd "M-RET") 'dss/indent-defun)
(define-key paredit-mode-map (kbd "C-M-s") 'paredit-backward-up)
(define-key paredit-mode-map (kbd "C-M-k") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-j") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-\\")
  (lambda ()
    (interactive)
    (dss/indent-defun)
    (back-to-indentation)))

(defun dss/paredit-yank ()
  (interactive)
  (if (not mark-active)
      (progn
        (call-interactively 'yank)
        (if (and (looking-back "\)")
                 (looking-at-p "\("))
            (progn
              (reindent-then-newline-and-indent)
              (if (looking-at-p "^")
                  (newline)))))
    (call-interactively 'yank))
  (condition-case nil (dss/indent-defun)))

(define-key paredit-mode-map (kbd "C-y") 'dss/paredit-yank)

(defun dss/paredit-open-parenthesis ()
  (interactive)
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
        (t (paredit-open-parenthesis))))
(define-key paredit-mode-map "(" 'dss/paredit-open-parenthesis)

(defun dss/paredit-semicolon ()
  (interactive)
  (if (looking-at-p " +\(")
      (progn
        (search-forward "(")
        (backward-char)))
  (cond ((looking-at-p "[[:blank:]]*$")
         (self-insert-command 1))
        ((looking-back ";")
         (self-insert-command 1))
        ((equal last-command this-command)
         (undo)
         (self-insert-command 1))
        ((and (not mark-active) (looking-at-p "\("))
         (progn
           (mark-sexp)
           (paredit-comment-dwim)
           ;; (save-excursion (reindent-then-newline-and-indent))
           (indent-according-to-mode)))
        ((and (not mark-active)
              (looking-at-p "^[[:blank:]]*$"))
         (insert ";;; "))
        ((and (not mark-active)
              (and (looking-back "^[[:blank:]]*")
                   (looking-at-p "[[:blank:]]*$")))
         (insert ";; "))
        (t (paredit-semicolon))))
(define-key paredit-mode-map ";" 'dss/paredit-semicolon)

(defun dss/paredit-open-line ()
  (interactive)
  (save-excursion
    (reindent-then-newline-and-indent))
  (indent-according-to-mode))
(define-key paredit-mode-map (kbd "M-o") 'dss/paredit-open-line)

(define-key paredit-mode-map (kbd "C-M-y") 'dss/replace-sexp)
(define-key global-map (kbd "C-M-y") 'dss/replace-sexp)

(defun dss/paredit-kill-ring-save ()
  (interactive)
  (if (not mark-active)
      (save-excursion
        (if (looking-at-p " +\(")
            (progn
              (search-forward "(")
              (backward-char)))
        (mark-sexp)
        (call-interactively 'kill-ring-save))
    (call-interactively 'kill-ring-save)))

(define-key paredit-mode-map (kbd "M-w") 'dss/paredit-kill-ring-save)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skeletons for elisp

(define-skeleton dss/elisp-let-skeleton
  "A simple e-lisp let skeleton"
  nil
  "(let ((" @ - "))" \n >
  @ _ ")")

(setq dss-let-skeleton-func 'dss/elisp-let-skeleton)

(define-skeleton dss/elisp-defun-skeleton
  "A simple e-lisp defun skeleton"
  nil
  "(defun dss/" @ - " (" @ ")" \n >
  "(interactive" @ ")" \n >
  @ _
    ")")
(setq dss-defun-skeleton-func 'dss/elisp-defun-skeleton)

(define-skeleton dss-elisp-progn-skeleton
  "A simple e-lisp progn skeleton"
  nil
  "(progn" @ - \n >
  @ _ ")"
    (dss/indent-defun))
(setq dss-progn-skeleton-func 'dss-elisp-progn-skeleton)

(define-skeleton dss/elisp-setq-skeleton
  "A simple e-lisp setq skeleton"
  nil
  "(setq " @ - " " @ _ ")")


(defun dss/elisp-s-or-setq ()
  (interactive)
  (dss/paredit-char-or-func 'dss/elisp-setq-skeleton t))
(define-key emacs-lisp-mode-map "s" 'dss/elisp-s-or-setq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magical context-sensitive additions to paredit

(defun dss/in-slime-repl-p ()
  (equal mode-name "REPL"))

(defun dss/paredit-char-or-func (func &optional undo-on-repeat)
  (cond ((looking-back "[[:alnum:]-]")
         (self-insert-command 1))
        ((equal last-command this-command)
         (when undo-on-repeat
           (undo))
         (self-insert-command 1))
        ((and (not (or (dss/in-comment-p)
                       (dss/in-string-p)))
              (or
               (looking-at-p "\(")
               (and (looking-back "^[[:blank:]]*")
                    (looking-at-p "[[:blank:]]*$"))
               (and (looking-back "^[[:blank:]]*")
                    (looking-at-p "[[:blank:]]*\("))))
         (funcall func))
        (t (self-insert-command 1))))


(defun dss/paredit-l-or-sexp-wrap-let ()
  (interactive)
  (cond ((looking-back "[[:alnum:]-]")
         (self-insert-command 1))
        ((equal last-command this-command)
         (undo)
         (self-insert-command 1))
        ((and (not (dss/in-string-p))
              (looking-at-p "\("))
         (if (not mark-active)
             (mark-sexp))
         (funcall dss-let-skeleton-func))
        (t (self-insert-command 1))))

(defun dss/paredit-d-or-defun ()
  (interactive)
  (cond ((and (not (looking-at-p "\("))
              (looking-back "[[:alnum:]-]"))
         (self-insert-command 1))
        ((equal last-command this-command)
         (undo)
         (self-insert-command 1))
        ((and (not (dss/in-string-p))
              (or (and (dss/in-slime-repl-p)
                       (slime-repl-at-prompt-start-p))
                  (looking-at-p "^")))
         (if (and (not (looking-at "\(def"))
                  (looking-at "\("))
             (if (not mark-active)
                 (mark-sexp)))
         (funcall dss-defun-skeleton-func)
         (dss/indent-defun))
        (t (self-insert-command 1))))

(defun dss/paredit-9-or-paren ()
  (interactive)
  (cond ((and (looking-back "\(")
              (looking-at "\)"))
         (paredit-open-parenthesis))
        ((equal last-command this-command)
         (cond ((looking-back "9")
                (self-insert-command 1))
               (t
                (undo)
                (insert " ")
                (backward-char 1)
                (paredit-open-parenthesis))))
        ((and (not (dss/in-string-p))
              (or (looking-at-p "^")
                  (looking-at-p "[\(\"#\\[{]")
                  (and (dss/in-slime-repl-p)
                       (slime-repl-at-prompt-start-p))))
         (dss/paredit-open-parenthesis))
        (t (self-insert-command 1))))

(defun dss/paredit-p-or-progn ()
  (interactive)
  (cond ((looking-back "[[:alnum:]-]")
         (self-insert-command 1))
        ((equal last-command this-command)
         (undo)
         (self-insert-command 1))
        ((and (not (dss/in-string-p))
              (looking-at-p "\(")
              (looking-back " "))
         (if (not mark-active)
             (mark-sexp))
         (funcall dss-progn-skeleton-func))
        (t (self-insert-command 1))))

(defun dss/paredit-y-or-yank ()
  (interactive)
  (cond ((or (and (looking-back " ")
                  (looking-at-p "\)"))
             (looking-back "^"))
         (call-interactively 'yank))
        (t (self-insert-command 1))))


(defun dss/paredit-c-or-copy ()
  (interactive)
  (cond (mark-active
         (call-interactively 'kill-ring-save))
        ((or (dss/in-string-p)
             (dss/in-comment-p)
             (looking-back "c"))
         (self-insert-command 1))
        ((equal last-command this-command)
         (self-insert-command 1))
        ((looking-at "\(def")
         (save-excursion
           (forward-char 1)
           (dss/copy-defun-name)))
        ((looking-at-p "[\(\"\\[{]")
         (dss/paredit-kill-ring-save))
        (t (self-insert-command 1))))

(defun dss/paredit-space-or-mark-sexp (&optional arg)
  (interactive "P")
  (when (and (looking-at-p "[[:blank:]]*")
             (looking-back "^[[:blank:]]*"))
    (back-to-indentation))
  (cond (arg (self-insert-command 1))
        ((and (looking-at-p "\(")
              (looking-back "[[:alnum:]]"))
         (insert " ")
         (backward-char 1))
        ((looking-back "[[:alnum:]]")
         (self-insert-command 1))
        ((and (not (dss/in-string-p))
              (looking-at-p "[\(\"#\\[{]"))
         (mark-sexp nil t))
        (t (self-insert-command 1))))


(defun dss/paredit-g-deactivate-mark ()
  (interactive)
  (cond (mark-active
         (setq mark-active nil))
        (t (self-insert-command 1))))

(defun dss/paredit-r-or-raise ()
  (interactive)
  (cond ((looking-back "[[:alnum:]-]")
         (self-insert-command 1))
        ((equal last-command this-command)
         (undo)
         (self-insert-command 1))
        ((and (not (or (dss/in-comment-p)
                       (dss/in-string-p)))
              (looking-at-p "\("))
         (paredit-raise-sexp))
        (t (self-insert-command 1))))

(defun dss/paredit-o-or-out ()
  (interactive)
  (cond ((looking-back "[ou]")
         (self-insert-command 1))
        ((equal last-command this-command)
         (pop-to-mark-command)
         (self-insert-command 1))
        ((or (looking-at-p "[\(\\[{]")
             (and (looking-back "^[[:blank:]]*")
                  (looking-at-p " *[\(#\\[{]")))
         (push-mark)
         (paredit-backward-up))
        (t (self-insert-command 1))))

(defun dss/paredit-u-or-out ()
  (interactive)
  (dss/paredit-o-or-out))


(defun dss/paredit-hypen-or-outermost ()
  (interactive)
  (cond ((and (looking-back "^ *")
              (looking-at-p " *[\(#\\[{]"))
         (dss/out-sexp nil))
        (t (self-insert-command 1))))
(defun dss/paredit-slash-or-jump-sexp ()
  (interactive)
  (cond ((looking-back "/")
         (self-insert-command 1))
        ((equal last-command this-command)
         (pop-to-mark-command)
         (self-insert-command 1))
        ((looking-at-p "[\(#\\[{]")
         (push-mark)
         (call-interactively 'dss/goto-match-paren))
        ((looking-back "[\]}\)]")
         (push-mark)
         (call-interactively 'dss/goto-match-paren))
        (t (self-insert-command 1))))

(defun dss/paredit-j-or-jump-sexp ()
  (interactive)
  (dss/paredit-slash-or-jump-sexp))

(defun dss/paredit-7-or-isearch-backward ()
  (interactive)
  (dss/paredit-char-or-func 'isearch-backward))

(defun dss/paredit-8-or-isearch-forward ()
  (interactive)
  (dss/paredit-char-or-func 'isearch-forward))

(defun dss/paredit-tab ()
  (interactive)
  (cond ((looking-back "^[[:blank:]]*")
         (indent-for-tab-command))
        ((looking-at-p "[[:alnum:]-/]")
         (back-to-indentation))
        ((looking-back "[[:alnum:]-/]")
         (lisp-complete-symbol))
        (t (back-to-indentation))))

(define-minor-mode dss-paredit+-mode
  "A helper mode for paredit"
  nil ;; initial-value
  " P+" ;; modeline
  '(
    ("l" . dss/paredit-l-or-sexp-wrap-let)
    ("d" . dss/paredit-d-or-defun)
    ("p" . dss/paredit-p-or-progn)
    ("y" . dss/paredit-y-or-yank)
    ("c" . dss/paredit-c-or-copy)
    (" " . dss/paredit-space-or-mark-sexp)
    ("g" . dss/paredit-g-deactivate-mark)
    ("r" . dss/paredit-r-or-raise)
    ("o" . dss/paredit-o-or-out)
    ("u" . dss/paredit-u-or-out)
    ("-" . dss/paredit-hypen-or-outermost)
    ("/" . dss/paredit-slash-or-jump-sexp)
    ("j" . dss/paredit-j-or-jump-sexp)
    ("7" . dss/paredit-7-or-isearch-backward)
    ("8" . dss/paredit-8-or-isearch-forward)
    ("9" . dss/paredit-9-or-paren)
    ("\t" . dss/paredit-tab))
  :group 'dss)

(define-key paredit-mode-map (kbd "<f4>=")
  '(lambda ()
     (interactive)
     (dss-paredit+-mode (if dss-paredit+-mode -1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; elisp

(defun dss/remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid.
  Comes from the emacs starter kit"
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))
            nil t))

(defun dss/elisp-mode-hook ()
  (dss/remove-elc-on-save)
  (eldoc-mode 1)
  (setq mode-name "EL:")
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-dabbrev)))
(add-hook 'emacs-lisp-mode-hook 'dss/elisp-mode-hook)

(defun dss/find-function-at-point ()
  "Find directly the function at point in the current window."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))

(define-key emacs-lisp-mode-map (kbd "M-.") 'dss/find-function-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; real lisp hackers use the lambda character
;; courtesy of stefan monnier on c.l.l
(defun sm-lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
          (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun dss/pretty-lambda-enable ()
  (interactive)
  (add-hook 'dss-lisp-modes-hook 'sm-lambda-mode-hook)
  (add-hook 'python-mode-hook 'sm-lambda-mode-hook))

(defun dss/pretty-lambda-disable ()
  (interactive)
  (remove-hook 'dss-lisp-modes-hook 'sm-lambda-mode-hook)
  (remove-hook 'python-mode-hook 'sm-lambda-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-lisps)
