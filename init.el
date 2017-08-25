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
;; Set paths to our manually installed Org-mode
(add-to-list 'load-path (expand-file-name "vendor/org-mode/lisp"
					  my-user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/org-mode/contrib/lisp"
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

(defun my-tangle-config-org ()
  "This function will write all source blocks from =config.org= into =config.el= that are ...
- not marked as =tangle: no=
- doesn't have the TODO state =DISABLED=
- have a source-code of =emacs-lisp="
  (require 'org)
  (let* ((body-list ())
         (output-file (concat my-user-emacs-directory "config.el"))
         (org-babel-default-header-args (org-babel-merge-params org-babel-default-header-args
                                                                (list (cons :tangle output-file)))))
    (message "—————• Re-generating %s …" output-file)
    (save-restriction
      (save-excursion
        (org-babel-map-src-blocks (concat my-user-emacs-directory "config.org")
	  (let* ((org_block_info (org-babel-get-src-block-info 'light))
		 ;;(block_name (nth 4 org_block_info))
		 (tfile (cdr (assq :tangle (nth 2 org_block_info))))
		 (match_for_TODO_keyword))
	    (save-excursion
	      (catch 'exit
		(org-back-to-heading t)
		(when (looking-at org-outline-regexp)
		  (goto-char (1- (match-end 0))))
		(when (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
		  (setq match_for_TODO_keyword (match-string 1)))))
	    (unless (or (string= "no" tfile)
			(string= "DISABLED" match_for_TODO_keyword)
			(not (string= "emacs-lisp" lang)))
	      (add-to-list 'body-list (concat "\n\n;; #####################################################################################\n"
					      "(message \"config • " (org-get-heading) " …\")\n\n"))
	      (add-to-list 'body-list body)))))
      (with-temp-file output-file
	;; Thanks for http://irreal.org/blog/?p=6236 and https://github.com/marcowahl/.emacs.d/blob/master/init.org for the read-only-trick:
        (insert ";; config.el --- This is the GNU/Emacs config file of Piers Cawley. -*- lexical-binding: t; eval: (read-only-mode 1) -*-\n")
        (insert ";; ======================================================================================\n")
        (insert ";; Don't edit this file, edit config.org' instead ...\n")
        (insert ";; Auto-generated at " (format-time-string current-date-time-format (current-time)) "on host " system-name "\n")
        (insert ";; ======================================================================================\n\n")
        (insert (apply 'concat (reverse body-list))))
      (message "—————• Wrote %s" output-file))))


;; The following lines are executed only when my-tangle-config-org-hook-func
;; was not invoked when saving config.org, which is the normal case
(let ((orgfile (concat my-user-emacs-directory "config.org"))
      (elfile (concat my-user-emacs-directory "config.el"))
      (gc-cons-threshhold most-positive-fixnum))
  (when (or (not (file-exists-p elfile))
	    (file-newer-than-file-p orgfile elfile))
    (my-tangle-config-org))
  (load-file elfile))

;; when config.org is saved, re-generate config.el
(defun my-tangle-config-org-hook-func ()
  (when (string= "config.org" (buffer-name))
    (let ((orgfile (concat my-user-emacs-directory "config.org"))
	  (elfile (concat my-user-emacs-directory "config.el")))
      (my-tangle-config-org))))
(add-hook 'after-save-hook 'my-tangle-config-org-hook-func)

(message "→★ loading init.el in %.2fs" (float-time (time-subtract (current-time) my-init-el-start-time)))

(setq custom-file my-custom-file)
(load custom-file)




