(require 'dss-paths)
(require 'ediff)
(require 'ediff-vers)
(require 'vc)
(require 'magit)
(require 'monky)

(setq magit-remote-ref-format 'remote-slash-branch)

;;; the builtin versions are buggy when the remote is anything other than origin/..
;;; http://stackoverflow.com/questions/2016901/viewing-unpushed-git-commits
(magit-define-inserter unpulled-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpulled "Unpulled commits:" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list "..@{u}")))))

(magit-define-inserter unpushed-commits (remote branch)
  (when remote
    (apply #'magit-git-section
           'unpushed "Unpushed commits:" 'magit-wash-log "log"
           (append magit-git-log-options
                   (list "@{u}..")))))

(setq dvc-tips-enabled nil)
(setq vc-follow-symlinks t)
(define-key magit-mode-map (kbd "<") 'magit-pull)

(defun vc-ediff ()
  (interactive)
  (vc-buffer-sync)
  (ediff-load-version-control)
  (setq ediff-split-window-function 'split-window-horizontally)
  (ediff-vc-internal "" ""))

(defun dss/vc-state-refresh-open-buffers ()
  (interactive)
  (message "updating buffer VC status ...")
  (mapc (lambda (b)
          (dss/vc-state-refresh (buffer-file-name b)))
        (buffer-list))
  (message ""))

(defun dss/vc-state-refresh (file &optional backend)
  (interactive)
  (when (> (length file) 0)
    (setq backend (or backend (vc-backend file)))
    (when backend
      (vc-state-refresh file backend))))

(defun dss/vc-choose-target ()
  (interactive)
  (ido-completing-read
   "which: "
   (split-string (shell-command-to-string "_vc_targets"))))
;;;
(defun dss/git-rehitch ()
  (interactive)
  (let ((hitch-status
         (apply #'format
                (cons "git hitch: '%s' - '%s'"
                      (mapcar
                       (lambda (var)
                         ;; (setenv var nil)
                         (let ((val (dss/local-shell-command-to-string
                                     (concat "echo -n $" var))))
                           (if (not (string-equal "" val))
                               (setenv var val)
                             (setq process-environment
                                   (setenv-internal process-environment
                                                    var nil nil)))
                           val))
                       '("GIT_AUTHOR_NAME" "GIT_AUTHOR_EMAIL"))))))
    (message hitch-status)
    hitch-status))

(defun dss/magit-status-hook ()
  (interactive)
  )
;; (defun dss/magit-status-hook ()
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-buffer)
;;     (insert (dss/git-rehitch))
;;     (insert "\n")))

(add-hook 'magit-refresh-status-hook 'dss/magit-status-hook)

(defun dss/magit-or-monky ()
  (interactive)
  (if (magit-get-top-dir default-directory)
      (call-interactively 'magit-status)
    (dvc-status)
    ;; (monky-status)
    ))

;; (setq monky-process-type 'cmdserver)
(setq monky-process-type nil)
;(define-key monky-queue-mode-map)
;; (setq monky-hg-style-log-graph
;;       (monky-get-style-path "log-graph")
;;                                         ;"/home/tavis/mercurial-cli-templates/map-cmdline.sglog"
;;       )


(provide 'dss-vc)
