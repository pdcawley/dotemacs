(eval-when-compile (require 'pdc-utils))

(req-package yasnippet
  :ensure t
  :if (not noninteractive)              ; no point in batch mode
  :bind
  (("C-c y TAB" . yas-expand)
   ("C-c y C-TAB" . yas-next-field)
   ("C-c y <C-tab>" . yas-next-field)
   ("C-c y n" . yas-new-snippet)
   ("C-c y f" . yas-find-snippets)
   ("C-c y r" . yas-reload-all)
   ("C-c y v" . yas-visit-snippet-file))
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

