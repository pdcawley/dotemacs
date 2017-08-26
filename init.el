;; =============================================================================
;; Bootstrap the real configuration, which is stored in config.org
;;
;; Basically lifted from https://github.com/novoid/dot-emacs
;; =============================================================================

;; Log starting time because it's nice to know EXACTLY how slow our
;; startup process is

(defconst my-init-el-start-time (current-time) 
  "Time when init.el was started")

;; user-emacs-directory is weird on Windows. Sod that
(defconst my-user-emacs-directory "~/.emacs.d/")
(defconst my-custom-file (expand-file-name "preferences.el"
                                           my-user-emacs-directory))
(defconst my-config-org (expand-file-name "config.org" my-user-emacs-directory))
;; Set paths to our manually installed Org-mode
(add-to-list 'load-path (expand-file-name "vendor/org-mode/lisp"
					  my-user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/org-mode/contrib/lisp"
                                          my-user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/req-package"
                                          my-user-emacs-directory))
(require 'org)

;; =============================================================================
;; The int.el looks for "config.org" and tangles its elisp blocks
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

(defun pdc//tangle-config-org ()
  "Writes all source blocks from =config.org= into =config.el= that are ...
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of emacs-lisp"
  (require 'org)
  (let ((body-list ())
        (visited? (get-file-buffer my-config-org))
        (output-file (concat my-user-emacs-directory "config.el"))
        (org-babel-default-header-args:emacs-lisp
         (org-babel-merge-params org-babel-default-header-args:emacs-lisp
                                 '((:tangle . "yes"))))
        to-be-removed)
    (prog1
        (save-restriction
          (save-window-excursion
            (find-file my-config-org)
            (setq to-be-removed (current-buffer))
            (dolist (spec (cdr (assoc "emacs-lisp" (org-babel-tangle-collect-blocks "emacs-lisp"))))
              (pcase-let*
                  ((`(,start ,file ,link ,source ,info ,body ,comment) spec)
                   (tangle (cdr (assq :tangle info)))
                   (todo-state (save-excursion
                                 (org-open-link-from-string link)
                                 (org-get-todo-state)))
                   
                   (disabled? (or (string= "DISABLED" (or todo-state ""))
                                  (string= tangle "no"))))
                (unless disabled?
                  (push (concat body "\n") body-list))))
          
            (with-temp-file output-file
              (insert ";; config.el -- This is the GNU/Emacs config file of Piers Cawley. -*- lexical-binding: t; eval: (read-only-mode 1) -*-\n
;;
;; Don't edit this file, edit config.org instead
;; Auto-generated at " (format-time-string current-date-time-format (current-time)) "on host " system-name "\n;;\n\n")
              (insert (apply 'concat (nreverse body-list)))
              (message "—————• Wrote %s" output-file))))
      (unless visited?
        (kill-buffer to-be-removed)))))

;; The following lines are executed only when my-tangle-config-org-hook-func
;; was not invoked when saving config.org, which is the normal case
(let ((orgfile (concat my-user-emacs-directory "config.org"))
      (elfile (concat my-user-emacs-directory "config.el"))
      (gc-cons-threshhold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
	    (file-newer-than-file-p orgfile elfile))
    (pdc//tangle-config-org))
  (load-file elfile))

;; when config.org is saved, re-generate config.el
(defun my-tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((orgfile (concat my-user-emacs-directory "config.org"))
	  (elfile (concat my-user-emacs-directory "config.el")))
      (pdc//tangle-config-org))))
(add-hook 'after-save-hook 'my-tangle-config-org-hook-func)

(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) my-init-el-start-time)))

(setq custom-file my-custom-file)
(load custom-file)




