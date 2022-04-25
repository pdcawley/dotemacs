;;; init.el --- -*- lexical-binding: t; -*-

;;
;;; By the time we get here, we should have use-package, which-key,
;;; general and a bunch of helper functions in scope
(pdc-initialize)

;; Look good!
(use-package doom-themes
  :config
  (load-theme 'doom-zenburn t)
  (custom-theme-set-faces
   'doom-zenburn
   '(shadow ((t (:foreground "#6F6F6F"))))))

(use-package dash)

(use-package projectile
  :init
  (setq projectile-mode-line-prefix " 🗁"
	projectile-known-projects-file (expand-file-name "projectile-booksmarks.eld"
							 pdc-local-dir)
	projectile-cache-file (expand-file-name "projectile.cache" pdc-cache-dir))
  (projectile-mode +1)
  :general
  (general-def :keymaps 'projectile-mode-map
    :prefix pdc-leader-key
    "P" '(projectile-command-map :which-key "Project")))

(use-package vertico
  :init
  (defvar +vertico/find-file-in--history nil)
  (defvar +vertico-consult-fd-args nil)

  (after! consult
    (setq +vertico-consult-fd-args consult-find-args)
    (defun +vertico/find-file-in (&optional dir initial)
      "Jump to file under DIR (recursive).
If INITIAL is non-nil, use as initial input."
      (interactive)
      (let* ((default-directory (or dir default-directory))
	     (prompt-dir (consult--directory-prompt "Find" default-directory))
	     (cmd (split-string-and-unquote +vertico-consult-fd-args " ")))
	(find-file
	 (consult--read
	  (split-string (cdr (apply #'pdc-call-process cmd)) "\n" t)
	  :prompt default-directory
	  :sort nil
	  :initial (if initial (shell-quote-argument initial))
	  :add-history (thing-at-point 'filename)
	  :category 'file
	  :history '(:input +vertico/find-file-in--history))))))
  (setq vertico-cycle t)
  (vertico-mode t))

(use-package marginalia
  :general
  (:keymaps 'minibuffer-local-map
   "M-A" 'marginalia-cycle)
  :init
  (marginalia-mode t))

(use-package embark
  :general
  ("C-." 'embark-act
   "C-;" 'embark-dwim)

  :init
  ;; (if (featurep 'which-key)
  ;;     (setq which-key--prefix-help-cmd-backup #'embark-prefix-help-command)
  ;;   (setq prefix-help-command #'embark-prefix-help-command))

  :config
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Copmletions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (after! which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
	(if (null keymap)
            (which-key--hide-popup-ignore-command)
	  (which-key--show-keymap
	   (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
	   (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
		 ((and (pred keymapp) km) km)
		 (_ (key-binding prefix 'accept-default)))
             keymap)
	   nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
	  '(embark-which-key-indicator
	    embark-highlight-indicator
	    embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
	(apply fn args)))

    (advice-add #'embark-completing-read-prompter
		:around #'embark-hide-which-key-indicator)))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :general
  ([remap isearch-forward] 'consult-line
   [remap switch-to-buffer] 'consult-buffer
   [remap switch-to-buffer-other-window] 'consult-buffer-other-window
   [remap repeat-complex-command] 'consult-complex-command
   [remap yank-pop] 'consult-yank-pop
   [remap apropos-command] 'consult-apropos
   [remap goto-line] 'consult-goto-line
   )
  (:keymaps 'isearch-mode-map
   "M-e" 'consult-isearch-history
   "M-s e" 'consult-isearch-history
   "M-s l" 'consult-line
   "M-s L" 'consult-line)
  (define-global
    "/" 'consult-line)
  (+general-global-menu! "buffer" "b"
    "b" 'consult-buffer
    "4" 'consult-buffer-other-window)
  (+general-global-menu! "jump" "j"
    "e" 'consult-compile-error
    "g" 'consult-goto-line
    "M-g" 'consult-goto-line
    "m" 'consult-mark
    "k" 'consult-global-mark
    "i" 'consult-imenu
    "I" 'consult-imenu-multi)
  (+general-global-menu! "search" "s"
    "f" 'consult-find
    "F" 'consult-locate
    "g" 'consult-grep
    "G" 'consult-git-grep
    "r" 'consult-ripgrep
    "l" 'consult-line
    "L" 'consult-line-multi
    "m" 'consult-multi-occur
    "u" 'consult-focus-lines
    "e" 'consult-isearch-history)
  (+general-global-menu! "register" "r"
    "#" #'consult-register-load
    "'" #'consult-register-store
    "C-#" #'consult-register)
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init

  (setq completion-in-region-function
	(fn! (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args)))

  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)

  (advice-add #'register-preview :override #'consult-register-window)

  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<")

  (after! flycheck
    (+general-global-jump "f" 'consult-flycheck))
  (after! org
    (+general-global-jump "o" 'consult-org-heading)))

(after! company
  (use-package consult-company
    :general
    (general-def
      :keymaps 'company-mode-map
      [remap completion-at-point] #'consult-company)))


(use-package consult-projectile
  :after (consult projectile)
  :general
  (+general-global-menu! "Project" "P"
    "f" '(consult-projectile :which-key "find-file-in-project")))

(use-package multiple-cursors
  :general
  (+general-global-menu! "multiple-cursors" "m"
    "l" (wk-cmd!! "Edit lines" #'mc/edit-lines)
    "n" (wk-cmd!! "Mark next" #'mc/mark-next-like-this)
    "N" (wk-cmd!! "Unmark next" #'mc/unmark-next-like-this)
    "p" (wk-cmd!! "Mark previous" #'mc/mark-previous-like-this)
    "P" (wk-cmd!! "Unmark previous" #'mc/unmark-previous-like-this)
    "t" (wk-cmd!! "Mark all" #'mc/mark-all-like-this)
    "m" (wk-cmd!! "Mark all DWIM" #'mc/mark-all-like-this-dwim)
    "e" (wk-cmd!! "Edit line endings" #'mc/edit-ends-of-lines)
    "a" (wk-cmd!! "Edit line starts" #'mc/edit-beginnings-of-lines)
    "s" (wk-cmd!! "Mark tag" #'mc/mark-sgml-tag-pair)
    "d" (wk-cmd!! "Mark in defun" #'mc/mark-all-like-this-in-defun)
    "<mouse-1>" (wk-cmd!! "Add cursor w/mouse" #'mc/add-cursor-on-click))
  :init
  (setq iedit-toggle-key-default nil))

(after! multiple-cursors-core
  (setq mc/list-file (expand-file-name "mc-lists.el" pdc-etc-dir)))

(use-package macrostep)

(use-package lsp-mode
  :general
  (after! lsp-mode
    (+general-global-code
      "a" 'lsp-execute-code-action
      "o" 'lsp-organize-imports
      "r" 'lsp-rename)))

(use-package consult-lsp
  :after lsp-mode
  :general
  (+general-global-code
    "j" (wk-cmd!! "Jump to symbol in current workspace" #'consult-lsp-symbols)
    "J" (wk-cmd!! "Jump to symbol in any workspace" #'consult-lsp-symbls 'all-workspaces)))

(use-package ripgrep)

(use-package diminish
  :demand t
  :config
  (after! gmch-mode
    (diminish 'gcmh-mode ""))
  (after! eldoc
    (diminish 'eldoc-mode ""))
  (setq auto-revert-mode-text " ⎌"))


(use-package which-key
  :diminish ""
  :config
  (which-key-mode))

(use-package racket-mode
  :general
  (+general-major-mode-def racket-mode
    "r" 'racket-run))


(use-package paredit
  :commands paredit-mode
  :diminish " ⓟ"
  :hook ((emacs-lisp-mode lisp-mode scheme-mode racket-mode racket-repl-mode) . enable-paredit-mode)
  :general
  (:keymaps 'paredit-mode-map
	    "DEL"	 '+pdc-paredit-backward-delete
	    "M-RET"	 '+pdc-indent-defun
	    "C-M-l"	 'paredit-recenter-on-sexp
	    "C-M-s"	 'paredit-backward-up
	    "C-M-\\"	 'pdc-indent-then-snap-to-indent
	    "C-M-y"	 'pdc-replace-sexp
	    "C-y"	 '+pdc-paredit-yank
	    "("		 '+pdc-paredit-open-parenthesis
	    ";"		 '+pdc-paredit-semicolon
	    "M-w"	 '+pdc-paredit-kill-ring-save
	    ")"		 '+pdc-paredit-close-round-and-newline
	    "M-)"	 'paredit-close-round
	    "M-k"	 'paredit-raise-sexp
	    "M-I"	 'paredit-splice-sexp)

  :config

  (defun pdc/out-sexp (&optional level forward syntax)
    "Skip out of any nested brackets.

Skip forward if FORWARD is non-nil, else backward.
If SYNTAX is non-nil, it is the state returned by `syntax-ppss' at point.
Return non-nil iff skipping was done."
    (interactive)
    (if (nth 3 (syntax-ppss))
	(beginning-of-thing 'sexp))
    (let* ((depth (syntax-ppss-depth (or syntax (syntax-ppss))))
	   (level (or level depth))
	   (forward (if forward -1 1)))
      (unless (zerop depth)
	(if (> depth 0)
	    ;; Skip forward out of nested brackets.
	    (condition-case ()		; Beware invalid syntax
		(backward-up-list (* forward level))
	      (error nil))
	  ;; Invalid sytntax (too many closed brackets).
	  ;; Skip out of as many as possible.
	  (let (done)
	    (while (condition-case nil
		       (progn (backward-up-list forward)
			      (setq done t))
		     (error nil)))
	    done)))))

  (defun pdc/indent-sexp ()
    "http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode.
Can be used from any coding major mode"
    (interactive)
    (save-restriction
      (save-excursion
	(widen)
	(let* ((inhibit-point-motion-hooks t)
	       (parse-status (syntax-ppss (point)))
	       (beg (nth 1 parse-status))
	       (end-marker (make-marker))
	       (end (progn (goto-char beg) (forward-list) (point))))
	  (set-marker end-marker end)
	  (goto-char beg)
	  (while (< (point) (marker-position end-marker))
	    ;; Don't reindent blank lines so we don't set the "buffer
	    ;; modified" property for nothing
	    (beginning-of-line)
	    (unless (looking-at "\\s-*$")
	      (indent-according-to-mode))
	    (forward-line))))))
  
  (defun +pdc-indent-defun ()
    (interactive)
    (save-excursion
      (pdc/out-sexp)
      (forward-char)
      (pdc/indent-sexp)))

  (defun +pdc-paredit-backward-delete ()
    (interactive)
    (if mark-active
	(call-interactively 'delete-region)
      (paredit-backward-delete)))

  (defun pdc/indent-defun-or-region ()
    (interactive)
    (if mark-active
	(call-interactively 'indent-region)
      (+pdc-indent-defun)))

  (defun +pdc-indent-then-snap-to-indent ()
    (interactive)
    (pdc/indent-defun-or-region)
    (back-to-indentation))

  (defun +pdc-paredit-yank ()
    (interactive)
    (call-interactively 'yank)
    (unless mark-active
      (when (and (looking-back ")" 1)
		 (looking-at "("))
	(reindent-then-newline-and-indent)
	(when (looking-at-p "^")
	  (newline))))
    (condition-case nil (+pdc-indent-defun)))

  (defun +pdc-paredit-kill-ring-save ()
    (interactive)
    (if (not mark-active)
	(save-excursion
	  (when (looking-at-p " +(")
	    (search-forward "(")
	    (backward-char))
	  (mark-sexp)
	  (call-interactively 'kill-ring-save))
      (call-interactively 'kill-ring-save)))

  (defun +pdc-paredit-open-parenthesis (&optional n)
    (interactive "P")
    (cond ((and (looking-back "(" 1)
		(looking-at ")"))
	   (paredit-open-parenthesis n))
	  ((equal last-command this-command)
	   (undo)
	   (insert " ")
	   (backward-char 1)
	   (paredit-open-parenthesis n))
	  ;; (nth 3 ...) = in-string-p
	  ((and (not (or mark-active (nth 3 (syntax-ppss))))
		(looking-at-p "[(a-z\"#\\[{]"))
	   (mark-sexp)
	   (paredit-open-parenthesis n)
	   (when (looking-at-p "[(\"#\\[{]")
	     (save-excursion (insert " "))))
	  ((paredit-open-parenthesis n))))
  
  (defun +pdc-paredit-semicolon (&optional n)
    (interactive "P")
    (when (looking-at-p "  +\(")
      (search-forward "(")
      (backward-char))
    (cond ((or (looking-back ";" 1)
               (looking-at-p "[[:blank:]]*$"))
           (self-insert-command 1))
          ((equal last-command this-command)
           (undo)
           (self-insert-command 1))
          ((and (not mark-active)
                (looking-at-p "^[[:blank:]]*$"))
           (insert ";;; "))
          ((and (not mark-active)
                (save-excursion
                  (beginning-of-line)
                  (looking-at-p "[[:blank:]]*$")))
           (insert ";; "))
          (t (paredit-semicolon n))))

  (defvar +paredit--post-close-keymap (make-sparse-keymap))
  (general-define-key :keymaps '(+paredit--post-close-keymap)
		      "SPC" (cmd!! #'just-one-space -1))

  (defun +pdc-paredit-close-round-and-newline ()
    (interactive)
    (paredit-move-past-close-and-newline ")")
    (set-transient-map +paredit--post-close-keymap))

  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map))


(use-package no-littering
  :commands
  (no-littering-expand-var-file-name
   no-littering-expand-etc-file-name)
  :init
  ;; Don't shit autosave files all over the place
  (setq auto-save-file-name-transforms
        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" ,(no-littering-expand-var-file-name "auto-save/\\2") t)
          (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Put the server socket somewhere sensible
  (setq server-socket-dir (no-littering-expand-var-file-name "server/")))

;;; Snippets, because typing is for unhappy people

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode)
  :functions (yas-load-directory)
  :diminish (yas-minor-mode . " 🅨")
  :mode ("/snippets/" . snippet-mode)
  :general
  (:keymaps 'yas-minor-mode-map
	    "M-s-/" 'yas-next-field)
  :init
  ;; ~/.emacs.d/snippets is a dead letter
  (defvar yas-global-mode nil)
  (defvar +snippets-dir (no-littering-expand-etc-file-name "snippets/"))
  (add-to-list 'yas-snippet-dirs '+snippets-dir)
  (setq yas-triggers-in-field t
	yas-wrap-around-region t
	yas-verbosity 0)
  (add-to-list 'load-path +snippets-dir)
  
  (setq yas-prompt-functions '(yas-completing-prompt))
  (defadvice! +snippets--remove-duplicates-a (templates)
    :filter-return #'yas--all-templates
    (cl-delete-duplicates templates :test #'equal))
  (yas-global-mode +1))

(use-package yasnippet-snippets
  ;; Yasnippet no longer includes a list of snippets :(
  )

(use-package doom-snippets
  :straight
  (:host github :repo "hlissner/doom-snippets" :branch "master" :files ("*.el" "*"))
  :after yasnippet
  :init
  (require 'doom-snippets-lib)
  (doom-snippets-initialize))



(use-package aas
  :after doom-snippets
  :hook
  (org-mode . aas-activate-for-major-mode)
  (text-mode . aas-activate-for-major-mode)
  :config

  (defmacro +aas-set-snippets (mode &rest args)
    (declare (indent 1))
    `(progn
       ,@(--map `(aas-set-snippets ,mode ,@it)
		(-partition-before-item :cond args))))
  
  (defun string-looking-back (string)
    (ignore-error beginning-of-buffer
      (let ((prefix-str (buffer-substring-no-properties (- (point) (length string)) (point))))
	(string-equal prefix-str string))))
  
  (defun +aas-is-after-cond (string &optional is-regexp limit)
    (if is-regexp
	(lambda () (looking-back string (or limit 1))))
    (lambda () (string-looking-back string)))
  
  (defun +aas-guess-dash ()
    (interactive)
    (let ((dash (save-excursion
		  (backward-char)
		  (cond ((looking-at-p "-")
			 (delete-forward-char 1)
			 "–")
			((looking-at-p "–")
			 (delete-forward-char 1)
			 " — ")
			((looking-at-p "[0-9]")
			 "–")
			(t "-")))))
      (insert dash)))

  
  (defun +aas-expand-yasnippet-fn (&optional key)
    (interactive)
    (doom-snippets-expand :key (or key aas-transient-snippet-key)))
  (defun +aas-expand-snippet-fn (&optional parens func)
    (interactive)
    (yas-expand-snippet (format "\\%s%s$1\%s$0"
				(or func aas-transient-snippet-key))))
  (aas-set-snippets 'text-mode
    "-" #'+aas-guess-dash))


(use-package company
  :hook (after-init . global-company-mode)
  :diminish " ⟴"
  :init
  (setq company-minimum-prefix-length 2
	company-tooltip-limit 14
	company-tooltip-align-annotations t
	company-require-match 'never
	company-global-modes
	'(not erc-mode
	      message-mode
	      help-mode
	      gud-mode
	      vterm-mode)
	company-frontends
	'(company-pseudo-tooltip-frontend
	  company-echo-metadata-frontend)

	;; Buffer-local backends will be computed when loading a major mode, so
	;; only specify a global default here.
	company-backends '(company-capf)
	company-auto-commit nil

	;; Only search the current buffer for `company-dabbrev' (a backend that
	;; suggests text from your open buffers). This prevents Company from
	;; causing lag with a lot of open buffers
	company-dabbrev-other-buffers nil
	company-dabbrev-ignore-case nil
	company-dabbrev-downcase nil

	company-transformers '(company-sort-prefer-same-case-prefix))
  

  ;; Nicked from the Doom autoloads
  (defvar +company-backend-alist
    '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
      (prog-mode (company-capf :with company-yasnippet))
      (conf-mode (company-capf :with company-yasnippet) company-dabbrev-code))
    "An alist matching modes to company backends. The backends for any mode is built from this.")

  (defun set-company-backend! (modes &rest backends)
    "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
    (declare (indent defun))
    (dolist (mode (pdc-enlist modes))
      (if null (car backends)
	(setq +company-backend-alist
	      (delq (assq mode +company-backend-alist)
		    +company-backend-alist))
	(setf (alist-get mode +company-backend-alist)
	      backends))))
  
  (defun +company--backends ()
    (let (backends)
      (let ((mode major-mode)
	    (modes (list major-mode)))
	(while (setq mode (get mode 'derived-mode-parent))
	  (push mode modes))
	(dolist (mode modes)
	  (dolist (backend (append (cdr (assq mode +company-backend-alist))
				   (default-value 'company-backends)))
	    (push backend backends)))
	(delete-dups
	 (append (cl-loop for (mode . backends) in +company-backend-alist
			  if (or (eq major-mode mode)
				 (and (boundp mode)
				      (symbol-value mode)))
			  append backends)
		 (nreverse backends))))))

  (defun +company-init-backends-h ()
    "Set `company-backends' for the current buffer."
    (or (memq major-mode '(fundamental-mode special-mode))
	buffer-read-only
	;; (doom-temp-buffer-p (or (buffer-base-buffer) (current-buffer)))
	(setq-local company-backends (+company--backends))))

  (put '+company-init-backends-h 'permanent-local-hook t)

  ;;
;;; Commands

  (defun +company-has-completion-p ()
    "Return non-nil if a completion candidate exists at point."
    (when company-mode
      (unless company-candidates-length
	(company-manual-begin))
      (= company-candidates-length 1)))
  
  (defun +company/toggle-auto-completion ()
    "Toggle as-you-type code completion"
    (interactive)
    (require 'company)
    (setq company-idle-delay (unless company-idle-delay 0.2))
    (message "Auto completion %s"
	     (if company-idle-delay "enabled" "disabled")))

  (defun +company/complete ()
    "Bring up the copletion popup. If only one result, complete it."
    (interactive)
    (require 'company)
    (when (ignore-errors
	    (/= (point)
		(cdr (bounds-of-thing-at-point 'symbol))))
      (save-excursion (insert " ")))
    (when (and (company-manual-begin)
	       (= company-candidates-length 1))
      (company-complete-common)))

  (defun +company/dabbrev ()
    "Invokes `company-dabbrev-code' in prog-mode buffers and `company-dabbrev' elsewhere"
    (interactive)
    (if (derived-mode-p 'prog-mode)
	#'company-dabbrev-code
      #'company-dabbrev))

  (defun +company/whole-lines (command &optional arg &rest ignored)
    "`company-mode' completion backend that completes whole-lines, akin to vim's C-x C-l."
    (interactive (list 'interactive))
    (require 'company)
    (pcase command
      (`interactive (company-begin-backend '+company/whole-lines))
      (`prefix      (company-grab-line "^[\t\s]*\\(.+\\)" 1))
      (`candidates
       (all-completions
	arg
	(delete-dups
	 (split-string
	  (replace-regexp-in-string
	   "^[\t\s]+" ""
	   (concat (buffer-substring-no-properties (point-min) (line-beginning-position))
		   (buffer-substring-no-properties (line-end-position) (point-max))))
	  "\\(\r\n\\|[\n\r]\\)" t))))))

  (defun +company/dict-or-keywords ()
    "`company-mode' completion combining `company-dict' and `company-keywords'."
    (interactive)
    (require 'company-dict)
    (require 'company-keywords)
    (let ((company-backends '((company-keywords company-dict))))
      (call-interactively #'company-complete)))

  (defun +company/dabbrev-code-previous ()
    "TODO"
    (interactive)
    (require 'company-dabbrev)
    (let ((company-selection-wrap-around t))
      (call-interactively #'+company/dabbrev)
      (company-select-previous-or-abort)))

  :config
  (add-hook 'after-change-major-mode-hook #'+company-init-backends-h 'append)

  (after! eldoc
    (eldoc-add-command 'company-complete-selection
		       'company-complete-common
		       'company-capf
		       'company-abort)))

(after! company-files
  (add-to-list 'company-files--regexps "file:\\(\\(?:\\.\\{1,2\\}/\\|~/\\|/\\)[^\]\n]*\\)E

"))

(use-package company-dict
  :defer t
  :config
  (setq company-dict-dir (no-littering-expand-etc-file-name "dicts")))

(use-package company-box
  :diminish ""
  :hook (company-mode . company-box-mode))


(use-package consult-yasnippet)

;;; Magit is bloody amazing. Gitty stuff here.
(use-package magit
  :commands magit-status
  :general
  (+general-global-menu! "git" "g"
    "s" 'magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0"
	magit-branch-arguments nil
	magit-push-always-verify nil))


;;; Git gutter stuff
(use-package git-gutter+
  :diminish (git-gutter+-mode git-gutter-mode)
  :init
  (+general-global-menu! "toggle" "T"
   "g" 'git-gutter+-mode)
  (git-gutter+-mode t))

(use-package git-gutter-fringe+
  :config
  (git-gutter-fr+-minimal))

(use-package org
  :preface
  (defvar +org-babel-native-async-langs '(python)
    "Languages that will use `ob-comint' instead of `ob-async' for `:async'.")

  (defvar +org-babel-mode-alist
    '((c . C)
      (cpp . C)
      (C++ . C)
      (D . C)
      (elisp . emacs-lisp)
      (sh . shell)
      (bash . shell)
      (matlab . octave)
      (rust . rustic-babel)
      (amm . ammonite))
    "An alist mapping languages to babel libraries. This is necessary for babel
libraries (ob-*.el) that don't match the name of the language.

For example, (fish . shell) will cause #+begin_src fish blocks to load
ob-shell.el when executed.")

  (defvar +org-babel-load-functions ()
    "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

  (setq org-directory "~/Dropbox/MobileOrg")

  (setq org-indirect-buffer-display 'current-window
	org-eldoc-breadcrumb-separator " → "
	org-enforce-todo-dependencies t
	org-entities-user
	'(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-heading-line t
	org-hide-leading-stars t
	org-image-actual-width nil
	org-imenu-depth 6
	org-priority-faces
	'((?A . error)
	  (?B . warning)
	  (?C . success))
	org-startup-indented t
	org-tags-column 0
	org-use-sub-superscripts '{}
	org-startup-folded nil)
  (setq org-refile-targets
	'((nil :maxlevel . 3)
	  (org-agenda-files :maxlevel . 3))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-ellipsis " ▾")
  (setq org-modules
	'(org-crypt
	  org-bookmark))
  (setq org-babel-noweb-wrap-start "«"
	org-babel-noweb-wrap-end "»")
  
  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))

  (setq org-todo-keywords
	'((sequence
	   "TODO(t)"	; A task that needs doing & is ready to do
	   "PROJ(p)"	; A project, usually contains other tasks
	   "LOOP(r)"	; A recurring task
	   "STRT(s)"	; A task in progress
	   "WAIT(w)"	; Something external is holding this task up
	   "HOLD(h)"	; This task is paused/held because of me
	   "IDEA(i)"	; An unconfirmed and unapproved task or notion
	   "|"
	   "DONE(d)"			; Task successfully completed
	   "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
	  (sequence
	   "TOLEARN(T)"	; Song that's ready to learn (Lyrics/tune decided etc)
	   "RESEARCH(R)"		; Song that's being researched
	   "NOTION(N)"			; Suggested song
	   "LEARNING(L)"		; Song I'm actively learning
	   "|"
	   "LEARNED(D)"			; Song in repertoire
	   "NOPED(X)"))			; Decided agin it
	org-todo-keyword-faces
	'(("STRT" . +org-todo-active)
	  ("RESEARCH" . +org-todo-active)
	  ("LEARNING" . +org-todo-active)
	  ("HOLD"     . +org-todo-onhold)
	  ("WAIT"     . +org-todo-onhold)
	  ("PROJ"     . +org-todo-project)
	  ("KILL"     . +org-todo-cancel)
	  ("NOPED"    . +org-todo-cancel))
	)

  (defun +org-init-babel-h ()
    (setq org-src-preserve-indentation t
	  org-src-tab-acts-natively t
	  org-confirm-babel-evaluate nil
	  org-link-elisp-confirm-function nil
	  org-src-window-setup 'other-window)

    (after! ob
      (add-to-list 'org-babel-default-lob-header-args '(:sync)))

    (defadvice! +org-fix-newline-and-indent-in-src-blocks-a (&optional indent _arg _interactive)
      "Mimic `newline-and-indent' in src blocks w/ lang-appropriate indentation."
      :after #'org-return
      (when (and indent
		 org-src-tab-acts-natively
		 (org-in-src-block-p t))
	(org-babel-do-in-edit-buffer
	 (call-interactively #'indent-for-tab-command))))

    (after! ob-ditaa
      (let ((default-directory (org-find-library-dir "org-contribdir")))
	(setq org-ditaa-jar-path (expand-file-name "scripts/ditaa.jar")
	      org-ditaa-eps-jar-path (expand-file-name "scripts/DitaaEps.jar")))))

  
  (defun +org--babel-lazy-load (lang &optional async)
    (cl-check-type lang (or symbol null))
    (unless (cdr (assq lang org-babel-load-languages))
      (when async
	(require 'ob-async nil t))
      (prog1 (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
		 (require (intern (format "ob-%s" lang)) nil t)
		 (require lang nil t))
	(add-to-list 'org-babel-load-languages (cons lang t)))))

  (defun +org-init-babel-lazy-loader-h ()
    "Load babel libraries lazily when babel blocks are executed."

    (defadvice! +org--export-lazy-load-library-h ()
      "Lazy load a babel package when a block is executed during exporting."
      :before #'org-babel-exp-src-block
      (+org--babel-lazy-load-library-a (org-babel-get-src-block-info)))

    (defadvice! +org--src-lazy-load-library-a (lang)
      "Lazy load a babel package to ensure syntax highlighting."
      :before #'org-src--get-lang-mode
      (or (cdr (assoc lang org-src-lang-modes))
	  (+org--babel-lazy-load lang)))

    (defadvice! +org--babel-lazy-load-library-a (info)
      "Load babel libraries lazily when babel blocks are executed."
      :after-while #'org-babel-confirm-evaluate
      (let* ((lang (nth 0 info))
	     (lang (cond ((symbolp lang) lang)
			 ((stringp lang) (intern lang))))
	     (lang (or (cdr (assq lang +org-babel-mode-alist))
		       lang)))
	(+org--babel-lazy-load
	 lang (and (not (assq :sync (nth 2 info)))
		   (assq :async (nth 2 info))))
	t))

    (advice-add #'org-babel-do-load-languages :override #'ignore))

  (add-hook! 'org-load-hook
	     #'+org-init-babel-h
	     #'+org-init-babel-lazy-loader-h)

  (add-hook! 'org-mode-local-vars-hook #'eldoc-mode)

  (when (and (featurep 'org)
	     (not byte-compile-current-file))
    (run-hooks 'org-load-hook))
  :config
  (setf (alist-get "roud" org-link-abbrev-alist)
	"https://www.vwml.org/roudnumber/")
  (setf (alist-get "twitter" org-link-abbrev-alist)
	"https://twitter.com/")
  (plist-put org-format-latex-options :scale 1.5))

(use-package org-variable-pitch
  :if window-system
  :hook
  (org-mode . org-variable-pitch-minor-mode)
  :init
  (when window-system
    (custom-theme-set-faces
     'user
     '(org-verse ((t (:inherit (variable-pitch)))))
     '(org-quote ((t (:inherit variable-pitch))))
     '(org-indent ((t (:inherit (org-hide fixed-pitch))))))))



(use-package writeroom-mode
  :demand t
  :custom
  (writeroom-maximize-window t)
  :config
  (delq! 'writeroom-set-fullscreen writeroom-global-effects))

(use-package org-contrib
  :after org)

(use-package org-attach
  :after org
  :straight (:type built-in)
  :init
  (setq org-attach-store-link-p t
	org-attach-use-inheritance t)
  
  :commands (org-attach-new
	     org-attach-open
	     org-attach-open-in-emacs
	     org-attach-reveal-in-emacs
	     org-attach-url
	     org-attach-set-directory
	     org-attach-sync)
  :config
  (unless org-attach-id-dir
    (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory)))
  (after! projectile
    (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir))
  (org-link-set-parameters "attachment" :image-data-fun #'+org-inline-image-data-fn))

(use-package org-modern
  :after org
  :straight
  (:type git :host github :repo "minad/org-modern")
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :init
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"

        org-agenda-block-separator ?—
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "←⭠ now ───────────────────────────────────────────────"))


(use-package ox-hugo
  :after org)

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t
	org-roam-directory "~/Documents/RoamNotes"
	org-roam-completion-enverywhere t)

  (defvar +org-roam-templates-dir (expand-file-name "org-roam/templates/" user-emacs-directory)
    "Where we org roam templates")

  (setq org-roam-capture-templates
	`(("d" "default" plain
	   "%?" :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	   :unnarrowed t)
	  ("p" "Project" plain
	   (file ,(expand-file-name "ProjectTemplate.org" +org-roam-templates-dir))
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project\n")
	   :unnarrowed t)
	  ("s" "Song" plain
	   (file ,(expand-file-name "SongTemplate.org" +org-roam-templates-dir))
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Song\n")
	   :unnarrowed t)
	  ("b" "Book note" plain
	   (file ,(expand-file-name "BookNoteTemplate" +org-roam-templates-dir))
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "+title: ${title}\n")
	   :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
	'(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

  :general
  (+general-global-menu! "roam" "n r"
    "a" (wk-cmd!! "Open random node"	#'org-roam-node-random)
    "A" (wk-cmd!! "Add an alias"        #'org-roam-alias-add)
    "f" (wk-cmd!! "Find node"		#'org-roam-node-find)
    "F" (wk-cmd!! "Find ref"		#'org-roam-ref-find)
    "g" (wk-cmd!! "Show graph"		#'org-roam-graph)
    "i" (wk-cmd!! "Insert"		#'org-roam-node-insert)
    "I" `((lambda (arg &rest args)
	    (interactive "P")
	    (let ((args (cons arg args))
		  (org-roam-capture-templates
		   (list (append (car org-roam-capture-templates)
				 '(:immediate-finish t)))))
	      (apply			#'org-roam-node-insert args)))
	  :which-key "Insert (skipping capture)")
    "n" (wk-cmd!! "Capture to node"	#'org-roam-capture)
    "P" (wk-cmd!! "Find Roam Project"   #'+org-roam-find-project)
    "r" (wk-cmd!! "Toggle roam buffer"	#'org-roam-buffer-toggle)
    "R" (wk-cmd!! "Launch roam buffer"  #'org-roam-buffer-display-dedicated)
    "s" (wk-cmd!! "Sync database"       #'org-roam-db-sync)
    "t" (wk-cmd!! "Tag"			#'org-roam-tag-add)
    "T" (wk-cmd!! "Un-Tag"		#'org-roam-tag-delete)
    "d" '(:ignore t :which-key "by date")
    "d b" (wk-cmd!! "Goto previous note" #'org-roam-dailies-goto-previous-note)
    "d d" (wk-cmd!! "Goto date" #'org-roam-dailies-goto-date)
    "d D" (wk-cmd!! "Capture date" #'org-roam-dailies-capture-date)
    "d f" (wk-cmd!! "Goto next note" #'org-roam-dailies-goto-next-note)
    "d m" (wk-cmd!! "Goto tomorrow" #'org-roam-dailies-goto-tomorrow)
    "d M" (wk-cmd!! "Capture tomorrow" #'org-roam-dailies-capture-tomorrow)
    "d n" (wk-cmd!! "Capture today" #'org-roam-dailies-capture-today)
    "d t" (wk-cmd!! "Goto today" #'org-roam-dailies-goto-today)
    "d T" (wk-cmd!! "Capture today" #'org-roam-dailies-capture-today)
    "d y" (wk-cmd!! "Goto yesterday" #'org-roam-dailies-goto-yesterday)
    "d Y" (wk-cmd!! "Capture yesterday" #'org-roam-dailies-capture-yesterday)
    "d -" (wk-cmd!! "Find directory" #'org-roam-dailies-find-directory))
  :config
  (require 'dash)
  (require 'org-roam-dailies)
  
  (defun +org-roam-capture-template (name-or-key)
    (--filter (string= name-or-key (nth (if (= 1 (length name-or-key)) 0 1) it))
	      org-roam-capture-templates))
    
  (defun +org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))

  (defun +org-roam-filter-by-level (level)
    (lambda (node)
      (= level (org-roam-node-level node))))

  (setf (symbol-function '+org-roam-filter-top-level)
	(+org-roam-filter-by-level 0))
  
  (defun +org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (+org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun +org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (+org-roam-list-notes-by-tag "Project")))

  (defun +org-roam-project-finalize-hook ()
    "Adds the captured template to `org-agenda-files' if the capture was not aborted."
    (remove-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)
  
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
	(add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun +org-roam-find-project ()
    (interactive)
    (add-hook 'org-capture-after-finalize-hook #'+org-roam-project-finalize-hook)
    (org-roam-node-find
     nil
     nil
     (-andfn (+org-roam-filter-by-level 0)
	     (+org-roam-filter-by-tag "Project"))
     :templates (+org-roam-capture-template "p")))
  (org-roam-db-autosync-mode t))

(after! org-roam
  (use-package deft
    :config
    (setq deft-directory org-roam-directory
	  deft-recursive t
	  deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
	  deft-use-filename-as-title t)
    :general
    (+general-global-notes
      "d" 'deft)))


(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-superstar
  :if window-system
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t))

;;; Jumping about with Avy

(use-package avy
  :general
  (+general-global-menu! "jump" "j"
    "j" 'avy-goto-char-timer
    "b" 'avy-goto-char
    "'" 'avy-goto-char-2
    "w" 'avy-goto-word-or-subword-1
    "s" 'avy-goto-symbol-1
    "SPC" 'avy-goto-whitespace-end))

(use-package emacs
  :diminish
  (auto-fill-function . " Ⓟ")
  (visual-line-mode . " 𝄙")
  :general
  (:keymaps 'vertico-map
	    "?" #'minibuffer-completion-help
	    "M-RET" #'minibuffer-force-complete-and-exit
	    "M-TAB" #'minibuffer-complete)
  (define-global
    "!" 'shell-command
    ":" 'execute-extended-command
    "C-;" 'eval-expression)
  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer)
  (+general-global-menu! "file" "f"
    "c" (wk-cmd!! "copy" #'write-file)
    "d" 'dired
    "f" 'find-file
    "g" 'consult-ripgrep
    "l" 'find-file-literally
    "s" 'save-buffer
    "v" '(:ignore t :which-key "variables")
    "v d" 'add-dir-local-variable
    "v f" 'add-file-local-variable
    "v p" 'add-file-local-variable-prop-line

    ;; "e" (list (cmd!! #'+vertico/find-file-in user-emacs-directory)
    ;; 	      :which-key "Find in .emacs.d")
    "E" (wk-cmd! "Find in .emacs.d" (+vertico/find-file-in user-emacs-directory))
    )
  
  :init
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (when EMACS28+
    (setq read-extended-command-predicate
	  #'command-completion-default-include-p))

  (when IS-MAC
    (defun pdc/yes-or-no-p (orig-fun &rest args)
      "Prevent `yes-or-no-p' from activating a dialog."
      (let ((use-dialog-box nil))
	(apply orig-fun args)))
    (advice-add #'yes-or-no-p :around #'pdc/yes-or-no-p)
    (advice-add #'y-or-n-p :around #'pdc/yes-or-no-p))

  (defconst +y-or-n-p-ret-yes-map
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map y-or-n-p-map)
      (define-key map [return] 'act)
      map)
    "A keymap for `y-or-n-p' with RET meaning \"yes\".")

  (defvar +y-or-n-p-ret-is-yes t
    "If true, `y-or-n-p' treats RET as \"yes\".")

  (defadvice! +y-or-n-p-handle-ret (fn &rest args)
    "Alters the treatment of RET in `y-or-n-p'."
    :around #'y-or-n-p
    (let ((y-or-n-p-map (if +y-or-n-p-ret-is-yes +y-or-n-p-ret-yes-map y-or-n-p-map)))
      (apply fn args)))
  

  (defalias 'yes-or-no-p #'y-or-n-p)

  (defvar pdc-fixed-width-font "Menlo")
  (defvar pdc-variable-pitch-font "ETBembo")
  (pcase window-system
    ('ns (custom-theme-set-faces
	  'user
	  `(default ((t (:family ,pdc-fixed-width-font :height 140))))
	  `(fixed-pitch ((t (:family ,pdc-fixed-width-font :height 140))))
	  `(variable-pitch ((t (:family ,pdc-variable-pitch-font :height 180 :weight thin)))))))
  
  (setq enable-recursive-minibuffers t)
  ;; Visual line stuff
  (setq visual-line-fringe-indicators '(nil right-curly-arrow)))
(global-visual-line-mode)

(use-package tree-sitter
  :hook (tree-sitter-after-on . tree-sitter-hl-mode)
  :config
  (global-tree-sitter-mode 1))

(use-package tree-sitter-langs
  :after tree-sitter
  :init
  (tree-sitter-langs-install-grammars)
  :config)


(use-package tree-sitter-indent)

(use-package json-mode)

(use-package combobulate
  :straight
  (:host github :repo "mickeynp/combobulate"))

(use-package restclient)

(use-package ob-restclient)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)));TODO:
 
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
