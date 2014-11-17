(require 'dss-lisps)
(require 'slime)
(slime-setup '(slime-repl))
(setq slime-protocol-version 'ignore)
(require 'slime-frame-colors)


(add-to-list 'load-path (concat dss-vendor-dir "ac-slime"))
(require 'ac-slime)
(defun dss/load-slime-completion()
  (interactive)
  (setq ac-sources (list
                    ac-source-slime-simple
                    ;;ac-source-slime-fuzzy
                    ac-source-words-in-buffer
                    ;;ac-source-filename
                    ac-source-dss-filename
                    ;;ac-source-words-in-same-mode-buffers
                    ;;ac-source-dictionary
                    )))

;;; I shouldn't need this stuff much longer as it should be in the
;;; next clojure-mode release:
(defun slime-tramp-local-filename (f)
  (interactive)
  (if (file-remote-p f)
      (tramp-file-name-localname
       (tramp-dissect-file-name f))
    f))

(defun slime-tramp-remote-filename (f)
  (interactive)
  (if (file-remote-p default-directory)
      (tramp-make-tramp-file-name
       (tramp-file-name-method
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-user
        (tramp-dissect-file-name default-directory))
       (tramp-file-name-host
        (tramp-dissect-file-name default-directory))
       f)
    f))

(defun slime-remote-file-name-hook ()
  (interactive)
    (setq slime-from-lisp-filename-function
      'slime-tramp-remote-filename)
    (setq slime-to-lisp-filename-function
      'slime-tramp-local-filename))

(add-hook 'slime-connected-hook 'slime-remote-file-name-hook)

(defun dss/slime-hook ()
  (interactive)
  (paredit-mode 1)
  ;; (define-key slime-mode-map " " 'dss/paredit-space-or-mark-sexp)
  (dss/load-slime-completion))

(add-hook 'slime-mode-hook 'dss/slime-hook)
(provide 'dss-slime)
