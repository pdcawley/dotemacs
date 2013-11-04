;;; 52haskell.el --- Custom haskell-mode configuration

;;(add-to-list 'load-path "~/lisp/haskell-mode")
(condition-case ()
    (progn
      (load "haskell-site-file")
      (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
      (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))
  (error nil))

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "(?\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))
         ("[\s\n]\\(->\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?→))))
         ("[\s\n]\\(<-\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?←))))
         ("[\s\n]\\(>=\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?≥))))
         ("[\s\n]\\(<=\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?≤))))
         ("[\s\n]\\(=>\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?⇒))))
         ("[\s\n]\\(\\.\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?∘))))
         ("[\s\n]\\(<<<\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?⋘))))
         ("[\s\n]\\(>>>\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?⋙))))
         ("[\s\n]\\(>>\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?≫))))
         ("[\s\n]\\(>>=\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    "≫="))))
         ("[\s\n]\\(<<\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?≪))))
         ("[\s\n]\\(-<\\)[\s\n]"
          (0 (progn (compose-region (match-beginning 1)
                                    (match-end 1)
                                    ?⤙)))))))


(when (window-system)
  (add-hook 'haskell-mode-hook 'pretty-lambdas-haskell))
