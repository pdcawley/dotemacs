(require 'flymake-shell)
(require 'dss-flymake)

(defun dss/sh-electric-pair ()
  ;; this version doesn't check to see if we're inside of a string or comment
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/sh-mode-hook ()
  (interactive)
  (define-key sh-mode-map (kbd "C-c v") 'dss/term-source-buffer)
  (define-key sh-mode-map (kbd "C-x C-e") 'dss/term-eval-region-or-para)
  (define-key sh-mode-map (kbd "<f4> d") 'dss/term-eval-region-or-para)

  (mapc (lambda (char)
          (progn
            (define-key sh-mode-map char 'dss/sh-electric-pair)))
        '("\"" "'" "(" "[" "{"))
  (puthash (buffer-file-name) 'flymake-shell-init
           dss-flymake-file-init-func-hash)
  (flymake-shell-load))
(add-hook 'sh-mode-hook 'dss/sh-mode-hook)
(provide 'dss-sh-mode)
