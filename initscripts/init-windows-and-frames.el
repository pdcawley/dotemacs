(setq frame-title-format '("" (:eval  (replace-regexp-in-string ":?[[:blank:]]*\\'" "" (format-mode-line mode-name)))
                           ": "
                           (:eval (if
                                      (buffer-file-name)
                                      (abbreviate-file-name
                                       (buffer-file-name))
                                    "%b"))

                           ))

