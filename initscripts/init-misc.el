;;; lexical-binding: t
;;; 90misc.el --- Miscellaneous definitions

;; Some custom commands

(defun set-tab-width (width)
  "Set tab-width to 4"
  (interactive "P")
  (set-variable 'tab-width width))

;; Browse kill-ring
(require 'browse-kill-ring)
(defadvice yank-pop (around kill-ring-browse-maybe (arg) activate)
  "If last action was not a yank, run `browse-kill-ring' instead."
  (if (not (eq last-command 'yank))
      (browse-kill-ring)
    ad-do-it))

;; My own magic mode
(require 'hbfc)
(hbfc-mode 1)

;; Can't `diminish' until after the modes are loaded...
(require 'diminish)
(defmacro pdc/after-loading (file &rest body)
  (declare (indent 1) (debug t))
  `(eval-after-load ,file '(progn ,@body)))

(pdc/after-loading "hideshow" (diminish 'hs-minor-mode))
(pdc/after-loading 'abbrev-mode (diminish 'abbrev-mode))
(pdc/after-loading 'hbfc-mode (diminish 'hbfc-mode))
(pdc/after-loading "undo-tree"
  (diminish 'undo-tree-mode))
(pdc/after-loading 'yasnippet (diminish 'yas-minor-mode))

;; Steve Yegge tips
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "FNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t   (rename-file name new-name 1)
              (rename-buffer new-name)
              (set-visited-file-name new-name)
              (set-buffer-modified-p nil)))))))

(bind-key "C-c R" 'rename-file-and-buffer)

;;
;; Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
     (directory:
      (if (string-match dir "\\(?:/\\|\\\\)$")
          (substring dir 0 -1) dir))
     (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; Ditch the chrome

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;;; TextMate style yank and indent - lifted from the EmacsWiki and tweaked for
;;; DRYness

(defvar pdc/programming-major-modes
  '(emacs-lisp-mode scheme-mode lisp-mode
    nxml-mode nxhtml-mode c-mode c++-mode
    objc-mode latex-mode plain-tex-mode
    js2-mode ruby-mode perl-mode cperl-mode)
  "List of modes where we should auto-indent on yank")

(defun pdc/indent-yanked-region ()
  "Reindent the yanked region.
Helper method for 'yank' advice"
  (if (member major-mode pdc/programming-major-modes)
      (let ((mark-even-if-inactive t))
        (indent-region (region-beginning) (region-end) nil))))

(defadvice yank (after indent-region activate)
  (pdc/indent-yanked-region))

(defadvice yank-pop (after indent-region activate)
  (pdc/indent-yanked-region))

;;; Buffer related stuff from the emacs starter kit

(defun ido-goto-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet
        ((add-name-and-pos (name position)
                           (unless (or (null name) (null position))
                             (add-to-list 'symbol-names name)
                             (add-to-list 'name-and-pos (cons name position))))
         (addsymbols (symbol-list)
                     (when (listp symbol-list)
                       (dolist (symbol symbol-list)
                         (let ((name nil) (position nil))
                           (cond
                            ((and (listp symbol) (imenu--subalist-p symbol))
                             (addsymbols symbol))

                            ((listp symbol)
                             (add-name-and-pos (car symbol) (cdr symbol)))

                            ((stringp symbol)
                             (add-name-and-pos symbol
                                               (get-text-property 1 'org-imenu-marker symbol)))))))))
      (addsymbols imenu--index-alist))
    (goto-char (cdr (assoc (ido-completing-read "Symbol? " symbol-names) name-and-pos)))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun recentf-ido-find-file ()
  "Find a recent file using ido"
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(\\_<lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook 'lisp-mode-hook 'pretty-lambdas)

(defun toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; Keyboard macros too handy to lose

(fset 'pdc:mxd-method->sub
      (lambda (&optional arg) "Keyboard macro." (interactive "p")
        (kmacro-exec-ring-item
         (quote ([1 M-right M-backspace 115 117 98 M-right
         134217820 11 32 123 return 109 121 25 backspace 61 32 64
         95 59 1 M-right right 36 115 101 108 102 44 32 1
         67108896 down 21 134217852 102 105 120 117 112 45 112 97
         114 97 109 115 return 19 109 101 116 104 111 100 1]
         0 "%d")) arg)))
(global-set-key [f6] 'pdc:mxd-method->sub)

;; helpers

(defun pdc/dasherize (string)
  (replace-regexp-in-string "_" "-" string))

(defun pdc/align-last-sexp ()
  (interactive)
  (save-excursion
    (backward-sexp)
    (let ((beg (point)))
      (forward-sexp)
      (align beg (point)))))

(defun pdc/align-has ()
  (interactive)
  (save-excursion
    (re-search-backward "has ")
    (re-search-forward "(")
    (backward-char)
    (forward-sexp)
    (pdc/align-last-sexp)))

(defun pdc/align ()
  (interactive)
  (save-excursion
    (cond ((region-active-p) (align (mark) (point)))
          (t
           (ignore-errors (up-list))
           (pdc/align-last-sexp)))))

(defun pdc/quote-last-sexp (&optional quote)
  (interactive)
  (or quote (setq quote "'"))
  (insert quote)
  (save-excursion
    (backward-char)
    (backward-sexp)
    (insert quote)))



(defun pdc/quote-behind (&optional quote)
  (interactive)
  (skip-chars-backward ",;[:blank:]")
  (or quote (setq quote "'"))
  (cond ((looking-back quote 1)
         (save-excursion
           (backward-sexp)
           (delete-char 1)
           (backward-sexp)
           (insert quote)))
        (t
         (pdc/quote-last-sexp quote))))

(defun pdc/quote-from-last-comma (&optional quote)
  (interactive)
  (or quote (setq quote "'"))
  (skip-chars-forward ",;[:blank:]")
  (insert quote)
  (save-excursion
    (fastnav-search-char-backward 1 ?,)
    (skip-chars-forward ",[:blank:]")
    (insert quote)))

(defun pdc/doublequote-behind ()
  (interactive)
  (pdc/quote-behind "\""))

;; (require 'annoying-arrows-mode)
;; (global-annoying-arrows-mode)


;; (aa-add-suggestion 'previous-line 'fastnav-jump-to-char-backward)
;; (aa-add-suggestion 'previous-line 'fastnav-sprint-backward)
;; (aa-add-suggestion 'next-line 'fastnav-jump-to-char-forward)
;; (aa-add-suggestion 'next-line 'fastnav-sprint-forward)
;; (aa-add-suggestion 'right-char 'fastnav-jump-to-char-forward)
;; (aa-add-suggestion 'right-char 'fastnav-sprint-forward)
;; (aa-add-suggestion 'left-char 'fastnav-jump-to-char-backward)
;; (aa-add-suggestion 'left-char 'fastnav-sprint-backward)
;; (aa-add-suggestion 'backward-delete-char-untabify
;;                    'fastnav-zap-up-to-char-backward)
;; (aa-add-suggestion 'backward-delete-char
;;                    'fastnav-zap-up-to-char-backward)

;; (add-annoying-arrows-advice
;;  delete-char
;;  '(subword-kill kill-line zap-to-char fastnav-zap-up-to-char-forward))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(bind-key "C-. C-<return>" 'toggle-window-split)

(defun pdc/use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defun pdc/default-for-read (&optional thing-type)
  (unless (pdc/use-region-p)
    (thing-at-point (or thing-type 'symbol))))

(defun isearch-yank-thing-at-point (thing-type)
  (let ((thing (thing-at-point thing-type)))
  (isearch-yank-string
   (with-temp-buffer
     (insert thing)
     (buffer-substring-no-properties (point-min) (point-max))))))

(defun isearch-yank-word-at-point ()
  "Yank the word at the current point"
  (interactive)
  (isearch-yank-thing-at-point 'word))

(defun isearch-yank-symbol-at-point ()
  "Yank the symbor at the current point"
  (interactive)
  (isearch-yank-thing-at-point 'symbol))

(define-key isearch-mode-map (kbd "A-w") 'isearch-yank-word-at-point)
(define-key isearch-mode-map (kbd "A-W") 'isearch-yank-symbol-at-point)

;;;_ , various loads

(req-package image-file
  :disabled t
  :init
  (auto-image-file-mode 1))

(req-package macrostep
  :ensure t
  :bind ("C-c e m" . macrostep-expand))

(req-package per-window-point
  :config
  (pwp-mode 1))


(defun bmi (weight height)
  (let ((pounds (* 1.0 (if (listp weight)
                           (+ (* 14 (car weight)) (cadr weight))
                         weight)))
        (inches (* 1.0 (if (listp height)
                           (+ (* 12 (car height)) (cadr height))
                         height))))
    (* 703.00 (/ pounds (* inches inches)))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (pp (eval (read (current-kill 0)))
          (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key "C-c e R" 'eval-and-replace)

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(when window-system
  (bind-key "S-<return>" 'smart-open-line)
  (bind-key "M-o" 'smart-open-line)
  (bind-key "M-O" 'smart-open-line-above))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of hte line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If point
reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun start-or-switch-to (function buffer-name)
  "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
Otherwise switch to the buffer named BUFFER-NAME. Don't clobber
the current buffer."
  (if (not (get-buffer buffer-name))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (funcall function))
    (switch-to-buffer-other-window buffer-name)))


(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (start-or-switch-to (lambda ()
                        'ansi-term (getenv "SHELL"))
                      "*ansi-term*"))


(defun visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running"
  (interactive)
  (start-or-switch-to 'ielm "*ielm*"))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
