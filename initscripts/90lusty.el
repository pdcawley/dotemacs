(use-package lusty-explorer
  :disabled t
  :bind ("C-x C-f" . lusty-file-explorer)
  :config
  (progn
    (add-hook 'lusty-setup-hook
              (lambda ()
                (bind-key "SPC" 'lusty-select-match lusty-mode-map)
                (bind-key "C-d" 'exit-minibuffer lusty-mode-map)))

    (defun lusty-open-this ()
      "Open the given file/directory/buffer, creating it if not already present."
      (interactive)
      (when lusty--active-mode
        (ecase lusty--active-mode
          (:file-explorer
           (let* ((path (minibuffer-contents-no-properties))
                  (last-char (aref path (1- (length path)))))
             (lusty-select-match)
             (lusty-select-current-name)))
          (:buffer-explorer (lusty-select-match)))))

    (defvar lusty-only-directories nil)

    (defun lusty-file-explorer-matches (path)
      (let* ((dir (lusty-normalize-dir (file-name-directory path)))
             (file-portion (file-name-nondirectory path))
             (files
              (and dir
                   ;; NOTE: directory-files is quicker but
                   ;;       doesn't append slash for directories.
                   ;;(directory-files dir nil nil t)
                   (file-name-all-completions "" dir)))
             (filtered (lusty-filter-files
                        file-portion
                        (if lusty-only-directories
                            (loop for f in files
                                  when (= ?/ (aref f (1- (length f))))
                                  collect f)
                          files))))
        (if (or (string= file-portion "")
                (string= file-portion "."))
            (sort filtered 'string<)
          (lusty-sort-by-fuzzy-score filtered file-portion))))

    (defun lusty-read-directory ()
      "Launch the file/directory mode of LustyExplorer."
      (interactive)
      (let ((lusty--active-mode :file-explorer))
        (lusty--define-mode-map)
        (let* ((lusty--ignored-extensions-regex
                (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
               (minibuffer-local-filename-completion-map lusty-mode-map)
               (lusty-only-directories t))
          (lusty--run 'read-directory-name default-directory ""))))

    (defun lusty-read-file-name ()
      "Launch the file/directory mode of LustyExplorer."
      (interactive)
      (let ((lusty--active-mode :file-explorer))
        (lusty--define-mode-map)
        (let* ((lusty--ignored-extensions-regex
                (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
               (minibuffer-local-filename-completion-map lusty-mode-map)
               (lusty-only-directories nil))
          (lusty--run 'read-file-name default-directory ""))))

    (if (featurep 'icicles)
        (defadvice lusty-file-explorer (around lusty-file-explorer-without-icy activate)
          (cl-flet ((message (&rest ignore)))
            (let ((icy-was-on icicle-mode))
              (if icy-was-on (icy-mode 0))
              (unwind-protect
                  ad-do-it
                (if icy-was-on (icy-mode 1)))))))))
