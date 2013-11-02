(use-package cc-mode
  :mode (("\\.h\\(h\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                  . c-mode)
         ("\\.mm\\'"                 . c++-mode))
  :init
  (progn
    (defun llvm-info ()
      (interactive)
      (w3m-find-file
  "/usr/local/opt/clang/docs/llvm/html/doxygen/classllvm_1_1IRBuilder.html"))

    (defun my-paste-as-check ()
      (interactive)
      (save-excursion
        (insert "/*\n")
        (let ((beg (point)) end)
          (yank)
          (setq end (point-marker))
          (goto-char beg)
          (while (< (point) end)
            (forward-char 2)
            (insert "CHECK: ")
            (forward-line 1)))
        (insert "*/n")))

    (defun my-c-indent-or-complete ()
      (interactive)
      (let ((class (syntax-class (syntax-after (1- (point))))))
        (if (or (bolp) (and (/= 2 class)
                            (/= 3 class)))
            (call-interactively 'indent-according-to-mode)
          (call-interactively 'auto-complete))))


    (defvar printf-index 0)



    (defun my-c-mode-common-hook ()
      (abbrev-mode 1)
      (gtags-mode 1)
      (hs-minor-mode 1)
      (hide-ifdef-mode )
      ;; (whitespace-mode 1)
      (which-function-mode 1)
      (auto-complete-mode 1)
      (yas-minor-mode)
      (bug-reference-prog-mode 1)

      (diminish 'gtags-mode)
      (diminish 'hs-minor-mode)
      (diminish 'hide-ifdef-mode)

      (bind-key "C-c P" 'insert-counting-printf c-mode-base-map)

      (auto-complete-mode 1)
      (setq ac-sources (list (if (and (fboundp 'semantic-active-p)
                                      (funcall #'semantic-active-p))
                                'ac-source-semantic
                               'ac-source-gtags)))
      (bind-key "<A-tab>" 'ac-complete c-mode-base-map)
      (bind-key "<return>" 'newline-and-indent c-mode-base-map)

      (set (make-local-variable 'yas-fallback-behavior)
           '(apply my-c-indent-or-complete . nil))
      (bind-key "<tab>" 'yas-expand-from-trigger-key c-mode-base-map)

      (unbind-key "M-j" c-mode-base-map)
      (bind-key "C-c C-i" 'c-includes-current-file c-mode-base-map)
      (bind-key "C-c C-y" 'my-paste-as-check c-mode-base-map)

      (set (make-local-variable 'parens-require-spaces) nil)
      (setq indicate-empty-lines t)
      (setq fill-column 72)

      (bind-key "M-q" 'c-fill-paragraph c-mode-base-map))

    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook))

  :config
  (progn
    (use-package cedet
      :disabled t
      :init
      (progn
        (dolist (submode '(global-semantic-idle-summary-mode
                           global-semantic-mru-bookmark-mode
                           global-semantic-idle-local-symbol-highlight-mode
                           global-semantic-show-unmatched-syntax-mode))
          (add-to-list 'semantic-default-submodes submode t))

        ;; Enable Semantic
        (semantic-mode 1)))))

(defun insert-counting-printf (arg)
  (interactive "P")
  (setq printf-index (if arg 1 (1+ printf-index)))
  (let ((fstr (cond ((string= mode-name "CPerl")
                     "say(STDERR 'step %d..');\n")
                    (t  "printf(\"step %d..\\n\");\n"))))
    (insert (format fstr printf-index))))
