(require 'dss-paths)
(require 'dss-elisp-funcs)
(defvar user-temporary-file-directory (concat dss-ephemeral-dir "tmp/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        ;; (,tramp-file-name-regexp nil)
        ))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; http://dorophone.blogspot.com/2011/11/how-to-make-emacs-scratch-buffer.html

(defvar persistent-scratch-filename
  (concat dss-ephemeral-dir ".emacs-persistent-scratch")
  "Location of *scratch* file contents for persistent-scratch.")

(defvar persistent-scratch-backup-directory
  (concat dss-ephemeral-dir "scratch-backups/")
  "Location of backups of the *scratch* buffer contents for
    persistent-scratch.")

(defun make-persistent-scratch-backup-name ()
  "Create a filename to backup the current scratch file by
  concatenating PERSISTENT-SCRATCH-BACKUP-DIRECTORY with the
  current date and time."
  (concat
   persistent-scratch-backup-directory
   (replace-regexp-in-string
    (regexp-quote " ") "-" (current-time-string))))

(defun save-persistent-scratch ()
  "Write the contents of *scratch* to the file name
  PERSISTENT-SCRATCH-FILENAME, making a backup copy in
  PERSISTENT-SCRATCH-BACKUP-DIRECTORY."
  (interactive)
  (with-current-buffer (switch-to-buffer "*scratch*")
    (if (file-exists-p persistent-scratch-filename)
        (copy-file persistent-scratch-filename
                   (make-persistent-scratch-backup-name)))
    (write-region (point-min) (point-max)
                  persistent-scratch-filename)))

(defun load-persistent-scratch ()
  "Load the contents of PERSISTENT-SCRATCH-FILENAME into the
  scratch buffer, clearing its contents first."
  (interactive)
  (if (file-exists-p persistent-scratch-filename)
      (with-current-buffer (switch-to-buffer "*scratch*")
        (delete-region (point-min) (point-max))
        (insert (dss/file-to-string persistent-scratch-filename)))))

(load-persistent-scratch)
(push #'save-persistent-scratch kill-emacs-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-tmp-files)
