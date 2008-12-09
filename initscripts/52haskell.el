;;; 52haskell.el --- Custom haskell-mode configuration

(add-to-list 'load-path "~/lisp/haskell-mode")
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(update-auto-mode-binding '("\\.hs\\'" . haskell-mode))
