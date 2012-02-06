(when (require 'rainbow-delimiters nil 'noerror)
  (mapc #'(lambda (hook)
            (add-hook hook 'rainbow-delimiters-mode))
        '(scheme-mode-hook
          clojure-mode-hook
          emacs-lisp-mode-hook)))
