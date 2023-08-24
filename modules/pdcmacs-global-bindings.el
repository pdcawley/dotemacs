;;; -*- lexical-binding: t; no-byte-compile: t; -*-
;;;
;;; pdcmacs-global-bindings.el -- Sets up our leader keys etc

;; Unbind annoyances

(use-package which-key)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(eval-when-compile
  (require 'general))

(define-prefix-command 'pdc-leader-map)
(general-def
  "M-m" 'pdc-leader-map)

(general-def
  "M-m s" '(:keymap search-map :which-key "search"))

(general-create-definer pdcmacs-leader-def
  :prefix "M-m")

(general-create-definer pdcmacs-mode
  :prefix "M-m ,")

(general-create-definer pdcmacs-app-def
  :prefix "M-m a" :prefix-map 'pdc-apps-map)


(general-create-definer pdcmacs-jump-def
  :prefix "M-m j" :prefix-map 'pdc-jump-map)


(defun pdc-toggle-window-dedication ()
  "Toggles window dedication in teh selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))


(defvar pdc-windows-key-map (make-sparse-keymap))
(pdcmacs-leader-def
  :infix "w"
  "" '(:keymap pdc-windows-key-map :which-key "windows")
  "<left>" 'winner-undo
  "<right>" 'winner-redo
  "d" 'pdc-toggle-window-dedication
  "u" 'winner-undo
  "n" 'windmove-down
  "p" 'windmove-up
  "b" 'windmove-left
  "f" 'windmove-right
  "<" 'scroll-left
  ">" 'scroll-right
  "}" 'enlarge-window-horizontally
  "{" 'shrink-window-horizontally
  "^" 'enlarge-window
  "v" 'shrink-window)

(pdcmacs-leader-def
  "!" 'shell-command
  ":" 'execute-extended-command
  "/" (cond ((fboundp 'consult-line) 'consult-line)
            ((fboundp 'swiper)       'swiper)
            (t                        'isearch-forward-regexp)))

;;; Universal argument stuff
(pdcmacs-leader-def "u" 'universal-argument)
(general-def :keymaps 'universal-argument-map
  "u" 'universal-argument-more)

;;; Window manipulation


;; Projects -- just lift C-x p for now
(pdcmacs-leader-def
  "p" '(:keymap project-prefix-map :wk "projects"))

;; Files
(defun pdc/find-emacs-init ()
  (interactive)
  (find-file pdcmacs-init-file))

(defun pdc/find-emacs-config ()
  (interactive)
  (find-file pdcmacs-config-file))

(defun pdc/dired-emacs-config-d ()
  (interactive)
  (dired user-emacs-directory))

(pdcmacs-leader-def
  :infix "f"
  :prefix-map 'pdc-files-map
  "" '(:ignore t :which-key "files")
  "f" 'find-file
  "F" 'dirvish-fd
  "s" 'save-buffer
  "w" 'write-file
  "e" '(:ignore t :which-key "emacs")
  "e i" '(pdc/find-emacs-init :wk "find init.el")
  "e c" '(pdc/find-emacs-config :wk "find config.el")
  "e d" '(pdc/dired-emacs-config-d :wk "find .emacs.d"))

(defun pdc/dired-sites ()
  (interactive)
  (dired (expand-file-name "Sites/" (getenv "HOME"))))

;; Directory stuff
(pdcmacs-leader-def
  :infix "d"
  :prefix-map 'pdc-dir-map
  "" '(:ignore t :which-key "dir")
  "4" 'dired-other-window
  "P" 'project-dired
  "d" 'dired
  "e" '(pdc/dired-emacs-config-d :wk ".emacs.d")
  "j" 'dired-jump
  "s" '(pdc/dired-sites :wk "~/Sites/"))

(for-gui
  (pdcmacs-leader-def
    :infix "d"
    "5" 'dired-other-frame
    "t" 'dired-other-tab))

  ;; Buffer stuff
34
(defvar pdc-buffers-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ctl-x-x-map)
    map))
(fset 'list-buffers 'ibuffer)

(pdcmacs-leader-def
  :infix "b"
  ""    '(:keymap pdc-buffers-map :wk "buffers")
  "b"   'switch-to-buffer
  "C-b" 'ibuffer
  "B"   'ibuffer
  "y"   'bury-buffer
  "n"   'next-buffer
  "p"   'previous-buffer
  "w"   'read-only-mode)

;; Apps
(pdcmacs-app-def
  ""    '(:ignore t :which-key "apps")
  "c"   'calc
  "C"   'calc-dispatch
  "p"   'list-processes
  "C-p" 'proced
  "u"   'undo-tree-visualize)

(general-def
  "M-m h" '(help-command :which-key "help"))

(defun make-inserter (c)
  `((lambda (&rest args) (interactive) (insert ,c))
    :wk ,(if (stringp c) c (string c))))

;;; C-x 8 helpers for stuff I type relatively often
(require 'iso-transl)
(general-define-key
      :keymaps 'iso-transl-ctl-x-8-map
      ". ," "…"
      ": )" "🙂"
      ": D" "😀"
      "; )" "😉"
      "\\"  "λ"
      "a ^" "↑"
      "a u" "↑"
      "a v" "↓"
      "a d" "↓"
      "a |" "↕")

;;; Set up a custom inputmode

(progn
 (quail-define-package
  "Pdc" "UTF-8" "😎" t
  "Custom input mode for my convenience"
  '(("\t" . quail-completion))
  t t nil nil nil nil nil nil nil t)

 (quail-define-rules
  (":)" ?🙂)
  (":P" ?😋)
  (":D" ?😀)
  (";)" ?😉)))

;;; Quitting emacs

(defun pdc/frame-killer ()
  "Kill server buffer and hide the main Emacs window."
  (interactive)
  (condition-case-unless-debug nil
      (delete-frame nil 1)
    (error (make-frame-invisible nil 1))))

(pdcmacs-leader-def :infix "q"
  "" '(nil :wk "quitting")
  "s" '(save-buffers-kill-emacs :wk "Save & Kill")
  "q" '(save-buffers-kill-terminal :wk "Save & disconnect")
  "Q" '(kill-emacs :wk "Quit!")
  "z" 'pdc/frame-killer)

;;; A few command name shortcuts. Blame Steve Yegge
(defalias 'qrr 'query-replace-regexp)
(defalias 'rr 'replace-regexp)

(pdcmacs-leader-def :keymaps 'lisp-mode-shared-map
  :infix "b"
  "v" '(eval-buffer :wk "eVal-buffer"))

(general-define-key :keymaps 'help-map
  :infix "a"
  ""    '(nil :which-key "apropos")
  "A"   'apropos
  "c"   'apropos-command
  "d"   'apropos-documentation
  "l"   'apropos-library
  "m"   'apropos-mode
  "o"   'apropos-user-option
  "v"   'apropos-variable
  "M-v" 'apropos-local-variable
  "V"   'apropos-value
  "M-V" 'apropos-local-value)

;;; A describe-foo leader
(dolist (key '(;; describe-*
               "b" "C-c" "C-o" "c" "C" "g" "I"
               "L" "m" "C-w" "P" "o" "s"
               ;; Apropos stuff
               "d"
               ;;  Info stuff
               "i" "F" "K" "r" "S"))
        (keymap-unset help-map key))

(general-define-key
 :keymaps 'help-map
 :infix "d"
 "" '(nil :wk "describe…")
 "b" 'describe-bindings
 "c" 'describe-key-briefly
 "C" 'describe-coding-system
 "f" 'describe-function
 "F" 'describe-face
 "i" 'describe-current-input-method
 "I" 'describe-input-method
 "k" 'describe-key
 "K" 'describe-keymap
 "o" '(describe-symbol :wk "describe-symbOl")
 "L" 'describe-language-environment
 "m" 'describe-mode
 "P" 'describe-package
 "s" 'describe-syntax
 "t" 'describe-theme
 "v" 'describe-variable
 "C-c" 'describe-current-coding-system)

(defun +info-elisp-manual ()
  "Display the Elisp manual in Info mode."
  (interactive)
  (info "elisp"))

(defun +info-emacs-manual ()
  "Display the Emacs manual in Info mode."
  (interactive)
  (info "emacs"))

(general-define-key
 :keymaps 'help-map
 :infix "i"
 "" '(nil :which-key "Info")
 "e" '+info-elisp-manual
 "i" 'info
 "F" 'Info-goto-emacs-command-node
 "K" 'Info-goto-emacs-key-command-node
 "r" '(+info-emacs-manual :wk "Info-emacs-manual")
 "S" 'info-lookup-symbol)

(pdcmacs-jump-def
  "$" 'move-end-of-line
  ">" 'end-of-buffer
  "<" 'beginning-of-buffer
  "a" 'back-to-indentation
  "[" 'beginning-of-defun
  "]" 'end-of-defun)

(use-package hydra)
(use-package kmacro
  :straight (kmacro :type built-in))

(pdcmacs-leader-def
  :infix "k"
  "" '(nil :wk "kill/macros")
  "k" (list
       (defhydra hydra-kill (nil nil :color red)
         "Kill"
         (";" kill-comment "comment")
         ("<tab>" delete-indentation "indentation")
         ("L" kill-whole-line "whole line")
         ("R" kill-rectangle "rectangle" :color blue)
         ("S" kill-sentence "sentence")
         ("l" kill-line "line")
         ("p" kill-paragraph "paragraph")
         ("r" kill-region "region")
         ("s" kill-sexp "sexp")
         ("v" kill-visual-line "visual-line")
         ("w" kill-word "word"))
       :which-key "kill")
  "K" (list kmacro-keymap :which-key "kmacro")
  "r R" 'replace-rectangle)

;;;
;;; Tweak the workings of `cycle-spacing', the new improved `just-one-space'
;;;

(setq cycle-spacing-actions '(just-one-space
                              (just-one-space -1)
                              delete-all-space
                              restore))

(provide 'pdcmacs-global-bindings)
