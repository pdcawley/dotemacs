;;; -*- lexical-binding: t; -*-

(eval-when-compile (require 'pdc-utils))

(req-package yasnippet
  :if (not noninteractive)              ; no point in batch mode
  :general
  (pdc|with-leader
   "y" '(nil "snippet")
   "y TAB" 'yas-expand
   "y C-TAB" 'yas-next-field
   "y <C-tab>" 'yas-next-field
   "y n" 'yas-new-snippet
   "y f" 'yas-find-snippets
   "y r" 'yas-reload-all
   "y v" 'yas-visit-snippet-file)
  :diminish (yas-minor-mode " Ⓨ")
  :commands
  (yas-minor-mode yas-expand)
  :mode
  ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :config
  (progn
    ; Load local snippets
    (yas-load-directory
     (expand-file-name "snippets/" user-emacs-directory))
    ; ... and use them everywhere
    (yas-global-mode t)))

(req-package autoinsert
  :requires yasnippet
  :init
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (auto-insert-mode 1)
  (defun pdc/autoinsert-yas-expand ()
    "Replace text in yasnippet template"
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))

  :config
  (define-auto-insert "\\.html?$" ["default-html.html" pdc/autoinsert-yas-expand])
  (define-auto-insert "\\.el$" [ "default-elisp.el" pdc/autoinsert-yas-expand ]))

