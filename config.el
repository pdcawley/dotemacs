;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'use-package))

(defun start-server-after-init ()
  (require 'server)
  (unless (server-running-p)
    (server-start)
    (midnight-mode +1)))

;;(use-feature eldoc-mode :diminish)

;;
;; Editing stuff

;; (use-package lispy
;;   :after (paredit embark)
;;   ;; TODO: Work out how to do this with 'bind'
;;   :config
;;   (keymap-unset lispy-mode-map "M-m")
;;   (keymap-unset lispy-mode-map "M-."))

                                        ;


(defgroup pdcmacs nil
  "Pdcmacs customization.")

(defcustom pdc-icon t
  "Display icons or not."
  :group 'pdcmacs
  :type 'boolean)


(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and window-system
       pdc-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))


;;
;; Completion frameworks and such.

(use-package vertico
  :after consult
  :custom
  (vertico-cycle t)
  (vertico-multiform-commands
   '((consult-recent-file buffer)
     (consult-mode-command buffer)
     (consult-complex-command buffer)
     (embark-bindings buffer)
     (consult-locate buffer)
     (consult-project-buffer buffer)
     (consult-ripgrep buffer)
     (consult-fd buffer)))
  :hook
  (after-init . vertico-mode)
  (after-init . vertico-multiform-mode)
  :bind
  (:map vertico-map
        :prefix "M-,"
        :prefix-map vertico-options-map
        ("r" . vertico-reverse-mode)
        ("g" . vertico-grid-mode))
  (:map vertico-map
        ("M-q"        . vertico-quick-insert)
        ("C-q"        . vertico-quick-exit)
        ("C-k"        . kill-whole-line)
        ("C-u"        . kill-whole-line)
        ("C-o"        . vertico-next-group)
        ("<tab>"      . vertico-insert)
        ("TAB"        . vertico-insert)
        ("M-<return>" . minibuffer-force-complete)))


(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-light nil)))


(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)
  :init
  (advice-add #'register-preview :override #'consult-register-window)

  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  :config
  (bind-keys ([remap isearch-forward] . consult-line)
             ([remap Info-search]        . consult-info)
             ([remap imenu]              . consult-imenu)
             ([remap recentf-open-files] . consult-recent-file)

             ("C-x M-:" . consult-complex-command)
             ("C-x b"   . consult-buffer)
             ("C-x 4 b" . consult-buffer-other-window)
             ("C-x 5 b" . consult-buffer-other-frame)
             ("C-x r b" . consult-bookmark)
             ("C-x p b" . consult-project-buffer)
             ("M-#"     . consult-register-load)
             ("M-'"     . consult-register-store)
             ("C-M-#"   . consult-register)
             ("M-y"     . consult-yank-pop)
             :map isearch-mode-map
             ("M-e" . consult-isearch-history)
             ("M-s e" . consult-isearch-history)
             ("M-s l" . consult-line)
             ("M-s L" . consult-line-multi)
             :map minibuffer-local-map
             ("C-s" ("insert-current-symbol" . (lambda ()
                                                 "Insert the current symbol"
                                                 (interactive)
                                                 (insert (save-excursion
                                                           (set-buffer (window-buffer (minibuffer-selected-window)))
                                                           (or (thing-at-point 'symbol t) ""))))))
             ("M-s" . consult-history)
             ("M-r" . consult-history)
             :map search-map
             ("d" . consult-find)
             ("D" . consult-locate)
             ("g" . consult-grep)
             ("G" . consult-git-grep)
             ("r" . consult-ripgrep)
             ("l" . consult-line)
             ("L" . consult-line-multi)
             ("k" . consult-keep-lines)
             ("u" . consult-focus-lines)
             ("e" . consult-isearch-history))
  (consult-customize
   consult-goto-line
   consult-theme :preview-key '(:debounce 0.4 any))
  :demand t)

(use-package aggressive-indent
  :init (aggressive-indent-global-mode t))

(use-package xref)

(use-package embark :after xref
  :bind
  (("C-." . embark-act)
   ("M-." . embark-act)
   ("M-," . embark-dwim)
   ("C-;" . embark-dwim)
   (([remap describe-bindings] . embark-bindings)))
  :custom
  (embark-cycle-key "M-.")
  (prefix-help-command #'embark-prefix-help-command))

(require 'rx)

;; (let ((parameters '(window-parameters . ((no-other-window . t)
;;                                          (no-delete-other-windows . t)))))

;;   (add-to-list 'display-buffer-alist
;;                '("\\*compilation\\*" display-buffer-no-window
;;                  (allow-no-window . t)))
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx (| "*info*"
;;                          "NEWS"))
;;                  (display-buffer-below-selected)
;;                  (window-height . fit-window-to-buffer)
;;                  ,parameters))
;;   (add-to-list
;;    'display-buffer-alist
;;    `(,(rx "*"
;;           "helpful " (*? anychar) ":" (* (not ?*))
;;           "*")
;;      display-buffer-in-side-window
;;      (side . bottom) (slot . 1) (window-width . fit-window-to-buffer)
;;      (window-parameters . ((no-other-window . 1)
;;                            (no-delete-other-windows . t)))))
;;   (add-to-list 'display-buffer-alist
;;                `(,(rx "*"
;;                       (| "xref"
;;                          "grep"
;;                          "Occur"
;;                          "Completions")
;;                       "*")
;;                  display-buffer-below-selected
;;                  (preserve-size . (nil . t))
;;                  ,parameters)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package ace-window)
(use-package 0x0)

(use-package dash
  :config
  (dash-enable-font-lock))
(use-package s)
(use-package f)
(use-package kv)
(use-package ht)

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'f)
  (require 'kv)
  (require 'ht))

;; Speedup with auto-compile
(use-package auto-compile
  :hook
  (after-init . auto-compile-on-load-mode)
  (after-init . auto-compile-on-save-mode)
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter t))

;;;
;;; Text wrangling

(defun +align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
if JUSTIFY-RIGHT is non nil justify to the right instead of the left. If AFTER is non-nil, add whitespace to the left instead of the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (message "%S" complete-regexp)
    (align-regexp start end complete-regexp group 1 t)))

(defmacro pdc|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "+align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) ,(if default-after t nil)))))
         (+align-repeat start end ,regexp ,justify-right after)))))

(defun +align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-regexp start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(require 'pdcmacs-global-bindings)

(pdc|create-align-repeat-x "comma"     "," nil t)
(pdc|create-align-repeat-x "semicolon" ";" nil t)
(pdc|create-align-repeat-x "colon"     ":" nil t)
(pdc|create-align-repeat-x "equal"     "=")
(pdc|create-align-repeat-x "math-oper" "[+\\-*/]")
(pdc|create-align-repeat-x "ampersand" "&")
(pdc|create-align-repeat-x "bar"       "|")
(pdc|create-align-repeat-x "left-paren" "(")
(pdc|create-align-repeat-x "right-paren" ")" t)
(pdc|create-align-repeat-x "backslash" "\\\\")
(pdc|create-align-repeat-x "quote" "['`]'")


(general-def
  :prefix "M-m |"
  ""   '(nil :which-key "align")
  ","  '+align-repeat-comma
  ";"  '+align-repeat-semicolon
  ":"  '+align-repeat-colon
  "="  '+align-repeat-equal
  "+"  '+align-repeat-math-oper
  "*"  '+align-repeat-math-oper
  "/"  '+align-repeat-math-oper
  "-"  '+align-repeat-math-oper
  "|"  '+align-repeat-bar
  "("  '+align-repeat-left-paren
  ")"  '+align-repeat-right-paren
  "\\" '+align-repeat-backslash
  "'"  '+align-repeat-quote
  "`"  '+align-repeat-quote)


;;; Repeat mode stuffs

(use-package repeat
  :disabled
  :custom
  (repeat-echo-function #'ignore)
  :config
  (repeat-mode t))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package yaml)

(use-package clipetty
  :diminish
  :hook (after-init . global-clipetty-mode))

;; (use-package yasnippet
;;   :commands (yas-global-mode yas-minor-mode)
;;   :functions (yas-load-directory)
;;   :diminish (yas-minor-mode . " ⓨ")
;;   :mode ("/yasnippet/snippets" . snippet-mode)
;;   :init
;;   (defvar yas-global-mode nil)
;;   (setopt yas-triggers-in-field t
;;           yas-wrap-around-region t
;;           yas-prompt-functions '(yas-completing-prompt))
;;   (defvar pdc-snippet-dirs (-filter 'file-directory-p
;;                                     (list (expand-file-name "snippets/" user-emacs-directory)
;;                                           (expand-file-name "~/.config/snippets"))))


;;   (setq yas-snippet-dirs pdc-snippet-dirs))


;; (use-package consult-yasnippet :after (consult yasnippet)
;;   :bind
;;   (("M-g y" . consult-yasnippet)))

;; ;; (use-package yasnippets-orgmode
;; ;;   :after org-mode)
;; (use-package yasnippets :after yasnippet
;;   :init
;;   (setq yas-snippet-dirs (cons (straight--el-get-package-directory 'yasnippets)
;;                                pdc-snippet-dirs))
;;   (yas-global-mode t))


(use-package visual-fill-column
  :defer nil
  :hook
  ((text-mode org-mode) . visual-fill-column-mode)
  :custom
  (visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-center-text t))

(for-terminal
  (xterm-mouse-mode 1))

(use-package powerline
  :hook
  (after-init . powerline-default-theme))

(use-package unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

;;; Movement/jumping
(defvar pdc-jump-map (make-sparse-keymap))

(use-package avy
  :bind
  (("M-m j j" . avy-goto-char-timer)
   ("M-m j b" . avy-goto-char)
   ("M-m j '" . avy-goto-char-2)
   ("M-m j w" . avy-goto-word-1)))

(use-package imenu
  :bind
  (("M-m j i" . imenu))
  :hook
  (font-lock-mode .  pdc/try-to-add-imenu)
  :custom
  (imenu-sort-function 'imenu--sort-by-name)
  :init
  (defun pdc/try-to-add-imenu ()
    "Add Imenu to modes that have font-lock-mode activated."
    (condition-case nil (imenu-add-to-menubar "Imenu")
      (error nil))))

(use-package imenu-list
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-auto-resize t)
  (imenu-list-position 'left)
  (imenu-list-size 40))

(use-package multiple-cursors
  :bind
  (:prefix "M-m m"
           :prefix-map pdc-multi-map
           :prefix-docstring "multi"
           ("a" . mc/edit-beginnings-of-lines)
           ("e" . mc/edit-ends-of-lines)
           ("^" . mc/edit-beginnings-of-lines)
           ("$" . mc/edit-ends-of-lines)
           ("m" . mc/edit-lines))
  :config)

(use-feature outline)

(use-feature ediff :after outline
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :hook
  (ediff-prepare-buffer . show-all)
  (ediff-quit           . winner-undo))

;; (use-package flycheck
;;   :commands global-flycheck-mode
;;   :diminish " ⓢ"
;;   :hook
;;   (after-init . global-flycheck-mode))

;;; Setup common lisp mode stuff

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     extempore-mode)
  "A list of Lisp style modes.")

(defvar lisp-mode-hooks
  (--map (intern (concat (symbol-name it) "-hook"))
         lisp-modes)
  "Hook variables associated with `lisp-modes'.")

(use-package calendar
  :custom
  (calendar-date-style 'iso))

(use-package editorconfig
  :diminish
  :hook
  (after-init . editorconfig-mode))

(use-package pdf-tools :if (display-graphic-p))

(use-feature notifications)

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :config
  (keymap-global-set "M-m d E" '("envrc" . envrc-command-map)))

(use-package json-ts-mode
  :mode ("\\.noisette\\'" "\\.json\\'"))

(use-package project :after envrc
  :bind (:map project-prefix-map
              ("C" . 'recompile)
              ("s" . 'consult-ripgrep))
  :config (dolist (func '(project-find-file project-find-dir project-eshell))
            (advice-add func :after #'envrc-allow)))

(use-package geiser-guile)

(use-package suggest
  :commands (suggest))

(use-package casual-dired
  :bind (:map dired-mode-map ("C-o" . 'casual-dired-tmenu)))
(use-package casual-info
  :bind (:map Info-mode-map ("C-o" . casual-info-tmenu)))
(setq font-lock-mode-hook (cdr font-lock-mode-hook))

(use-package vundo
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

(use-package haskell-mode)

(require 'pdcmacs-feeds)
(require 'pdcmacs-org)
(require 'pdcmacs-hugo-support)
(require 'pdcmacs-webservice)
