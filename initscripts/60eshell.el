;;; 60eshell.el --- Configuration and extension for eshell

;; eshell foo
(defun eshell-edit (&rest files)
  "Invoke `find-file' on all files."
  (if (listp (car files))
      (progn
        (let ((files2 (car files)))
          (while files2
            (find-file (pop files2)))))
    (while files
      (find-file (pop files)))))

(defalias 'eshell/emacs 'eshell-edit)
(defalias 'eshell/vi 'eshell-edit)

;;; Lifted from starter-kit-eshell

(setq eshell-cmpl-cycle-completions nil
      eshell-save-history-on-exit t
      eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")

(eval-after-load 'esh-opt
  '(progn
     (require 'em-prompt)
     (require 'em-term)
     (require 'em-cmpl)
     (setenv "PAGER" "cat")
     (add-hook 'eshell-mode-hook
               '(lambda () (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)))
     (add-to-list 'eshell-visual-commands "ssh")
     (add-to-list 'eshell-visual-commands "tail")
     (add-to-list 'eshell-command-completions-alist
                  '("gunzip" "gz\\'"))
     (add-to-list 'eshell-command-completions-alist
                  '("tar" "\\(\\.tar|\\.tgz|\\.tar\\.gz\\)\\'"))
     (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))

