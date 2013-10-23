;;; 47elisp.el --- Custom emacs-lisp-mode configuration

(require 'lisp-mode)
(use-package nukneval)
(use-package paredit
  :init (progn (add-hook 'lisp-mode-hook 'enable-paredit-mode)
               (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
               (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)))
(use-package redshank)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-zap-associated-elc)

(defun emacs-lisp-zap-associated-elc ()
  "If you're saving an elisp file, likely the .elc is no longer valid"
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\|)" . 'paren-face)))

(font-lock-add-keywords 'scheme-mode
                        '(("(\\|)" . 'paren-face)))

(defface paren-face
  '((((class color) (background dark))
     (:foreground "grey20"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face used to dim parentheses."
  :group 'pdc-faces)

(defun regen-autoloads ()
  "Regenerate the autoload definitions file if necessary and load it."
  (interactive)
  (if (or (not (file-exists-p autoload-file))
          ;; TODO: make this more readable
          (< (+ (car (nth 5 (file-attributes autoload-file))) 20)
             (car (current-time))))
      (let ((generated-autoload-file autoload-file))
        (message "Updating autoloads...")
        (update-directory-autoloads dotfiles-dir
                                    (concat dotfiles-dir "/elpa-to-submit"))))
  (load autoload-file))

(defun align-code (beg end &optional arg)
  (interactive "rP")
  (if (null arg)
      (align beg end)
    (let ((end-mark (copy-marker end)))
      (indent-region beg end-mark nil)
      (align beg end-mark))))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (progn
    (setq eval-expr-print-function 'pp
          eval-expr-print-level 20
          eval-expr-print-length 100)

    (defun eval-expr-minibuffer-setup ()
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (paredit-mode))))
