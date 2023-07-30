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

(general-create-definer pdcmacs-leader-def
  :prefix "M-m")

(general-create-definer pdcmacs-mode
  :prefix "M-m ,")

(general-create-definer pdcmacs-app-def
  :prefix "M-m a" :prefix-map 'pdc-apps-map)

(defvar pdc-windows-key-map (make-sparse-keymap))
(pdcmacs-leader-def
  :infix "w"
  "" '(:keymap pdc-windows-key-map :which-key "windows")
  "u" 'winner-undo
  "n" 'windmove-down
  "p" 'windmove-up
  "b" 'windmove-left
  "f" 'windmove-right)

(pdcmacs-leader-def
  "!" 'shell-command
  ":" 'execute-extended-command
  "/" (cond ((fboundp 'swiper)       'swiper)
            ((fboundp 'consult-line) 'consult-line)
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

(provide 'pdcmacs-global-bindings)
