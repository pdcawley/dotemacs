(use-package dired
  :defer t
  :init
  (progn
    (defvar mark-files-cache (make-hash-table :test #'equal))

    (defun mark-similar-versions (name)
      (let ((pat name))
        (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
            (setq pat (match-string 1 pat)))
        (or (gethash pat mark-files-cache)
            (ignore (puthash pat t mark-files-cache)))))

    (defun dired-mark-similar-version ()
      (interactive)
      (setq mark-files-cache (make-hash-table :test #'equal))
      (dired-mark-sexp '(mark-similar-versions name))))

  :config
  (progn
    (defun dired-package-initialize ()
      (unless (featurep 'runner)
        (use-package dired-x)
        (use-package runner)
        (use-package dired-details
          :commands dired-details-toggle)

        (bind-key "l" 'dired-up-directory dired-mode-map)
        (bind-key "H" 'dired-details-toggle dired-mode-map)
        (bind-key "W" 'wdired-change-to-wdired-mode dired-mode-map)

        (defun my-dired-switch-window ()
          (interactive)
          (if (eq major-mode 'sr-mode)
              (call-interactively #'sr-change-window)
            (call-interactively #'other-window)))

        (bind-key "<tab>" 'my-dired-switch-window dired-mode-map)

        (bind-key "M-!" 'async-shell-command dired-mode-map)

        (unbind-key "M-G" dired-mode-map)
        (unbind-key "M-s f" dired-mode-map)

        (defadvice dired-omit-startup (after dimiish-dired-omit activate)
          "Make sure to remove \"Omit\" from the modeline."
          (diminish 'dired-omit-mode) dired-mode-map)

        (defadvice dired-next-line (around dired-next-line+ activate)
          "Replace current buffer if file is a directory"
          ad-do-it
          (while (and (not (bobp)) (not ad-return-value))
            (forward-line -1)
            (setq ad-return-value (dired-move-to-filename)))
          (when (bobp)
            (call-interactively 'dired-next-line)))

        (defadvice dired-previous-line (around dired-previous-line+ activate)
          "Replace current buffer if file is a directory."
          ad-do-it
          (while (and (not (bobp)) (not ad-return-value))
            (forward-line -1)
            (setq ad-return-value (dired-move-to-filename)))
          (when (bobp)
            (call-interactively 'dired-next-line)))

        (defvar dired-omit-regexp ()
          (let ((file (expand-file-name ".git"))
                parent-dir)
            (while (and (not (file-exists-p file))
                        (progn
                          (setq parent-dir
                                (file-name-directory
                                 (directory-file-name
                                  (file-name-directory file))))
                          (not (string= (file-name-directory file)
                                        parent-dir))))
              ;; Move up to the parent dir and try again
              (setq file (expand-file-name ".git" parent-dir)))
            ;; If we found a change log in a parent, use that.
            (if (file-exists-p file)
                (let ((regexp (funcall dired-omit-regexp-orig))
                      (omitted-files
                       (shell-command-to-string "git clead -d -x -n")))
                  (if (= 0 (length omitted-files))
                      regexp
                    (concat
                     regexp
                     (if (> (length regexp) 0)
                         "\\|" "")
                     "\\("
                     (mapconcat
                      #'(lambda (str)
                          (concat
                           "^"
                           (regexp-quote
                            (substring str 13
                                       (if (= ?/ (aref str (1- (length str))))
                                           (1- (length str))
                                         nil)))
                           "$"))
                      (split-string omitted-files "\n" t)
                      "\\|")
                     "\\)")))
              (funcall dired-omit-regexp-orig))))))
    (add-hook 'dired-mode-hook 'dired-package-initialize)

    (defun dired-double-jump (first-dir second-dir)
      (interactive
       (list (ido-read-directory-name "First directory: "
                                      (expand-file-name "~")
                                      nil nil "dl/")
             (ido-read-directory-name "Second directory: "
                                      (expand-file-name "~")
                                      nil nil "Archives/")))
      (dired first-dir)
      (dired-other-window second-dir))

    (bind-key "C-c J" 'dired-double-jump)))

(use-package recentf
  :if (not noninteractive)
  :init
  (progn
    (recentf-mode 1)

    (defun recentf-add-dired-directory ()
      (if (and dired-directory
               (file-directory-p dired-directory)
               (not (string= "/" dired-directory)))
          (let ((last-idx (1- (length dired-directory))))
            (recentf-add-file
             (if (= ?/ (aref dired-directory last-idx))
                 (substring dired-directory 0 last-idx)
               dired-directory)))))

    (add-hook 'dired-mode-hook 'recentf-add-dired-directory)))
