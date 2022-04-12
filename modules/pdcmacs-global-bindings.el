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
(pdcmacs-leader-def
  :infix "a"
  :prefix-map 'pdc-apps-map
  ""    '(:ignore t :which-key "apps")
  "c"   'calc
  "C"   'calc-dispatch
  "p"   'list-processes
  "C-p" 'proced
  "u"   'undo-tree-visualize)

(general-def
  "M-m h" '(help-command :which-key "help"))

(provide 'pdcmacs-global-bindings)
