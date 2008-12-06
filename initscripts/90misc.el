;;; 90misc.el --- Miscellaneous definitions

;; Some custom commands

(defun set-tab-width (width)
  "Set tab-width to 4"
  (interactive "P")
  (set-variable 'tab-width width))

;; RFC fetching
(defun fetch-rfc (arg)
  (interactive "MRFC number: ")
  (let ((name (format "*rfc%s*" arg))
        (url (format "http://www.ietf.org/rfc/rfc%s.txt" arg)))
    (if (get-buffer name)
        (switch-to-buffer name)
      (switch-to-buffer (url-retrieve-synchronously url))
      (rename-buffer name)
      (goto-char (point-min))
      (while (and (not (looking-at "^$"))
                  (not (eobp)))
        (forward-line 1))
      (forward-line 1)
      (delete-char (- (1- (point))))
      (setq buffer-read-only t)
      (rfcview-mode)
      (not-modified))))

(add-hook 'rfcview-mode-hook 'llasram/rfcview-extra-keys)
(defun llasram/rfcview-extra-keys ()
  (define-key rfcview-mode-map [mouse-1] 'muse-follow-name-at-mouse)
  (define-key rfcview-mode-map [mouse-2]
    'muse-follow-name-at-mouse-other-window))

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

;; Set up some snippets
(require 'yasnippet)
(yas/define 'emacs-lisp-mode
            "gsk"
            "global-set-key (kbd \"$1\") $0"
            "global-set-key" nil)
(yas/define 'emacs-lisp-mode
            "dk"
            "define-key $1-map (kbd \"$2\") '$0"
            "define-key" nil)

;; Ditch the chrome

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

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
