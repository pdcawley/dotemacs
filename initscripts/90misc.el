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
(diminish 'abbrev-mode)
(diminish 'hs-minor-mode)
(diminish 'hbfc-mode)

;; Steve Yegge tips
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn   (rename-file name new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil))))))
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

;;; TextMate style yank and indent - lifted from the EmacsWiki and tweaked for DRYness

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
    (flet
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
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 M-right M-backspace 115 117 98 M-right 134217820 11 32 123 return 109 121 25 backspace 61 32 64 95 59 1 M-right right 36 115 101 108 102 44 32 1 67108896 down 21 134217852 102 105 120 117 112 45 112 97 114 97 109 115 return 19 109 101 116 104 111 100 1] 0 "%d")) arg)))
(global-set-key [f6] 'pdc:mxd-method->sub)

;; helpers

(defun pdc/dasherize (string)
  (replace-regexp-in-string "_" "-" string))

