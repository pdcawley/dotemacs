;;; core-keybinds.el -*- lexical-binding: t; -*-

;; Sets up `general', `which-key' etc and helper functions for our
;; leader key based key bindings.

;; Set up the various leaders we want for the rest of our keybinds.
(use-package dash)

(use-package general
  :preface
  (defvar pdc-leader-key "M-m"
    "Our leader key.")
  (defvar pdc-local-leader-key "M-,"
    "Mode specific leader key for Insert/Emacs mode")
  (defvar pdc-leader-map (make-sparse-keymap)
    "An overriding keymap for <leader> keys.")

  :init

  (defun pdc--major-mode-map (mode)
    (intern (replace-regexp-in-string "\\(-map\\)?$" "-map"
				      (if (symbolp mode)
					  (symbol-name mode)
					mode))))

  (general-create-definer define-global
    :prefix pdc-leader-key
    :keymaps 'override)
  (defmacro +general-global-menu! (name infix-key &rest body)
    (declare (indent defun))
    (let* ((force (and (eq infix-key :force)
		       (pop body)))
	   (infix-key (if force (pop body) infix-key))
	   (definer (intern (concat "+general-global-" name)))
	   (keymap (intern (concat "+general-global-" name "-map"))))

      `(progn
	 ,(if (and (fboundp definer)
		   (not force))
	      `(progn)
	    `(progn
	       (general-create-definer ,definer
		 :wrapping define-global
		 :prefix-map (quote ,keymap)
		 :infix ,infix-key
		 :wk-full-keys nil)
	       (,definer "" '(:ignore t :which-key ,name))))
	 (,definer
	   ,@body))))

  (general-create-definer define-major-mode-key
    :prefix pdc-local-leader-key
    :keymaps 'override
    :major-modes t)

  (defmacro +general-major-mode-def (mode &rest body)
    (declare (indent defun))
    (let ((maps (-map #'pdc--major-mode-map (pdc-enlist mode))))
      `(define-major-mode-key
	 :keymaps (quote ,maps)
	 ,@body))))


(when IS-MAC
  (setq mac-control-modifier 'control
	mac-right-control-modifier 'left
	mac-command-modifier 'super
	mac-right-command-modifier 'left
	mac-option-modifier 'meta
	mac-function-modifier 'none)

  (defun toggle|mac-right-option-modifier ()
    "Toggle between passing option modifier to Emacs or MacOS."
    (interactive)
    (unless (get 'mac-right-option-modifier 'pdc-initial-value)
      (put 'mac-right-option-modifier 'pdc-initial-value mac-right-option-modifier))
    (let ((initial (get 'mac-right-option-modifier 'pdc-initial-value))
	  (old mac-right-option-modifier))
      (if (eq mac-right-option-modifier initial)
	  (setq mac-right-option-modifier (if (eq old 'none) 'left 'none))
	(setq mac-right-option-modifier (if (eq old 'none) initial 'none)))
      (message "Toggled `mac-right-option-modifier' from %s to %s"
	       old mac-right-option-modifier)))

  (add-hook! 'after-init-hook
    (message "Fooby")
    (+general-global-toggle
      "M" '(:ignore t :which-key "Mac")
      "M O" '(toggle|mac-right-option-modifier :which-key "Right Opt")))
  

  ;; Don't use the dialog boxes.
  (defun +yes-or-no-p-a (orig-fun &rest args)
    "Prevent yes-or-no-p from activating a dialog."
    (let ((use-dialog-box nil))
      (apply orig-fun args)))
  (advice-add #'yes-or-no-p :around #'+yes-or-no-p-a)
  (advice-add #'y-or-n-p :around #'+yes-or-no-p-a))

;;; Setup our leader key menus if we're interactive
(when pdc-interactive-p
  (+general-global-menu! "code" "c"
    "c" 'compile
    "C" 'recompile
    "w" 'delete-trailing-whitespace)

  (+general-global-menu! "buffer" "b"
    "d"		'kill-current-buffer
    "b"		'switch-to-buffer
    "4"		'switch-to-buffer-other-window
    "B"		'list-buffers
    "C-b"	'list-buffers
    "y"         'bury-buffer)
  (+general-global-menu! "jump" "j"
    "c" 'goto-char
    "g" 'goto-line
    "i" 'imenu)
  (+general-global-menu! "search" "s"
    "l" 'locate
    "L" 'ffap-menu
    "g" 'ripgrep
    "o" 'multi-occur
    "m" 'bookmark-jump)
  (+general-global-menu! "insert" "i"
    "u" 'insert-char)
  (+general-global-menu! "open" "o"
    "b" 'browse-url-of-file
    "f" 'make-frame
    "-" 'dired-jump)
  (+general-global-menu! "quit/restart" "q"
    "f" 'delete-frame
    "K" 'save-buffers-kill-emacs
    "q" 'kill-emacs
    "Q" 'save-buffers-kill-terminal)
  (+general-global-menu! "toggle" "T"
    "c" 'global-display-fill-column-indicator-mode
    "F" 'toggle-frame-fullscreen
    "w" 'visual-line-mode)

  (+general-global-menu! "rect/register" "r"
    ;; Register stuff
    "g" 'insert-register
    "x" 'copy-to-register
    "j" 'jump-to-regsiter
    "SPC" 'point-to-register
    "+" 'increment-register
    "n" 'number-to-register
    "r" 'copy-rectangle-to-register
    "w" 'window-configuration-to-register
    "#" 'list-registers
    ;; Rectangle stuff
    "k" 'kill-rectangle
    "o" 'open-rectangle
    "y" 'yank-rectangle
    "c" 'clear-rectangle
    "d" 'delete-rectangle
    "s" 'string-rectangle
    "m" 'rectangle-mark-mode
    "M-w" 'copy-rectangle-as-kill
    "N" 'rectangle-number-lines
    )
  (+general-global-menu! "file" "f"
    "d" 'dired
    "f" 'find-file
    "l" 'locate
    "r" 'recentf-open-files)

  (+general-global-menu! "notes" "n")
  )


(provide 'core-keybinds)
