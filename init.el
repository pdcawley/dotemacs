;(package-initialize)

(setq package-enable-at-startup nil)
(require 'package)

(setq temporary-file-directory (expand-file-name "~/tmp/emacstmp"))
(unless (file-exists-p temporary-file-directory)
  (make-directory temporary-file-nidirectory t))

(defconst my-custom-file (expand-file-name "preferences.el"
                                           user-emacs-directory))

;;(setq debug-on-error t)
(setq custom-file my-custom-file)
(load my-custom-file t)
(setq auto-save-default nil)
(setq make-backup-files nil)
(put 'erase-buffer 'disabled nil)
(require 'zenburn-theme)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))
(defconst emacs-start-time (current-time))

(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

(defvar initscripts-d (expand-file-name "initscripts/" emacs-d)
  "The location of our init scripts")

;; (add-hook 'after-init-hook (lambda () (load (expand-file-name "init-real.el" emacs-d))))
(load (expand-file-name "init-real.el" emacs-d))

;; (load-file my-custom-file)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                        emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
        `(lambda ()
           (let ((elapsed (float-time (time-subtract (current-time)
                             emacs-start-time))))
         (message "Loading %s...done (%.3fs) [after-init]"
              ,load-file-name elapsed)))
        t))
