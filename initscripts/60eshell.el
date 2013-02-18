;;; 60eshell.el --- Configuration and extension for eshell

;; eshell foo
(defun eshell-edit (&rest files)
  "Invoke `find-file' on all files."
  (if (null files)
      (bury-buffer)
    (mapc #'find-file
          (mapcar #'expand-file-name
                  (eshell-flatten-list (reverse args))))))

(defalias 'eshell/emacs 'eshell-edit)
(defalias 'eshell/vi 'eshell-edit)

(defun perldoc (man-args)
  (interactive "sPerldoc: ")
  (require 'man)
  (let ((manual-program "perldoc"))
    (man man-args)))

(defun eshell/perldoc (&rest args)
  "Like `eshell/man', but invoke `perldoc'."
  (funcall 'perldoc (apply 'eshell-flatten-and-stringify args)))

(defun eshell-maybe-bol ()
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (if (= p (point))
        (beginning-of-line))))

(add-hook 'eshell-mode-hook
          '(lambda () (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

;; Needs fixing to interpret tramp prefixes.
(defun pdc/eshell-prompt ()
  (let ((dirs (split-string (eshell/pwd) "/")))
    (concat (getenv "USER") "@" (getenv "HOST") ":"
            ((lambda (p-lst)
               (if (> (length p-lst) 3)
                   (concat
                    (mapconcat (lambda (elm) (substring elm 0 1))
                               (butlast p-lst 3)
                               "/")
                    "/"
                    (mapconcat (lambda (elm) elm)
                               (last p-lst 3)
                               "/"))
                 (mapconcat (lambda (elm) elm)
                            p-lst
                            "/")))
             (if (equal (car dirs) "") (cdr dirs) dirs))
            (if (= (user-uid) 0) " # " " $ "))))

(setq eshell-prompt-function #'pdc/eshell-prompt)

(defun tyler-eshell-view-file (file)
  "A version of `view-file' which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
        (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
            'special)
        (progn
          (switch-to-buffer buffer)
          (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
                               (+ (window-point)
                                  (length (funcall eshell-prompt-function))))))
        (switch-to-buffer buffer)
        (view-mode-enter (cons (selected-window) (cons nil undo-window))
                         'kill-buffer)))))
(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
    the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (tyler-eshell-view-file file)
          (goto-line line))
      (tyler-eshell-view-file (pop args)))))

(defalias 'eshell/more 'eshell/less)

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
                  '("tar" "\\(\\.tar|\\.tgz|\\.tar\\.gz\\)\\'")))
;;  (add-to-list 'eshell-output-filter-functions 'eshell-handle-ansi-color)))
