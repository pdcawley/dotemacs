;; =============================================================================
;; Bootstrap the real configuration, which is stored in config.org
;;
;; Basically lifted from https://github.com/novoid/dot-emacs
;; =============================================================================

;; Log starting time because it's nice to know EXACTLY how slow our
;; startup process is


;(package-initialize)

(defconst my-init-el-start-time (current-time)
  "Time when init.el was started")

;; user-emacs-directory is weird on Windows. Sod that
(defconst my-user-emacs-directory "~/.emacs.d/")
(defconst my-custom-file (expand-file-name "preferences.el"
                                           my-user-emacs-directory))
(defconst my-config-org (expand-file-name "config.org" my-user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/use-package" my-user-emacs-directory))
(load-file (expand-file-name "litprog.el" my-user-emacs-directory))

;; =============================================================================
;; The init.el looks for "config.org" and tangles its elisp blocks
;; (matching the criteria described below) to "config.el" which is
;; loaded as Emacs configuration.
;; I got this from https://github.com/novoid/dot-emacs/blob/master/init.el
;; =============================================================================

(defvar current-date-time-format "%a %b %d %Y-%m-%dT%H:%M:%S "
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for suggested replacements")

(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision")

(let ((elfile (expand-file-name "config.el" my-user-emacs-directory)))
  (when (or (not (file-exists-p elfile))
            (file-newer-than-file-p my-config-org elfile))
    (org-babel-tangle-file my-config-org))
  (load-file elfile))

;;; When config.org is saved, regenerate config.el
;;;
;;; Possibly replace this with something to autotangle any .org file
;;; on save. Later.

(defun pdc/tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((elfile (expand-file-name "config.el" my-user-emacs-directory)))
      (org-babel-tangle-file my-config-org))))
(add-hook 'after-save-hook 'pdc/tangle-config-org-hook-func)

(message "→★ loading init.el in %.2fs"
         (float-time (time-subtract (current-time) my-init-el-start-time)))

(setq custom-file my-custom-file)
(load custom-file)
