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

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(defvar bootstrap-version)
(let* ((straight-repo-dir
        (expand-file-name "straight/repos" user-emacs-directory))
       (bootstrap-file
        (concat straight-repo-dir "/straight.el/bootstrap.el"))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (concat
      "mkdir -p " straight-repo-dir " && "
      "git -C " straight-repo-dir " clone "
      "https://github.com/raxod502/straight.el.git && "
      "git -C " straight-repo-dir " checkout 2d407bc")))
  (load bootstrap-file nil 'nomessage))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (setq comp-deferred-compilation t))


(straight-use-package 'general)
(straight-use-package 'use-package)
(use-package diminish)
(use-package bind-key)
(use-package which-key)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/.emacs.d/config.org" "/Users/pdcawley/Dropbox/org/CAH.org" "/Users/pdcawley/Dropbox/org/DwarfFortress.org" "/Users/pdcawley/Dropbox/org/GVT-and-config-interfaces.org" "/Users/pdcawley/Dropbox/org/Polymorphic-Dispatch.org" "/Users/pdcawley/Dropbox/org/Small-Emacs-Snippet.org" "/Users/pdcawley/Dropbox/org/TST-scratch-choir.org" "/Users/pdcawley/Dropbox/org/asynch-streams.org" "/Users/pdcawley/Dropbox/org/axes-maths-theremins.org" "/Users/pdcawley/Dropbox/org/bakery-lob-sources.org" "/Users/pdcawley/Dropbox/org/barehanded.org" "/Users/pdcawley/Dropbox/org/bbc.org" "/Users/pdcawley/Dropbox/org/bil-brewster.org" "/Users/pdcawley/Dropbox/org/blog.org" "/Users/pdcawley/Dropbox/org/building.org" "/Users/pdcawley/Dropbox/org/client-info-meeting-minutes.org" "/Users/pdcawley/Dropbox/org/client-info-thoughts.org" "/Users/pdcawley/Dropbox/org/codex.org" "/Users/pdcawley/Dropbox/org/coding.org" "/Users/pdcawley/Dropbox/org/diary.org" "/Users/pdcawley/Dropbox/org/efe-live-chat.org" "/Users/pdcawley/Dropbox/org/facts.org" "/Users/pdcawley/Dropbox/org/fko-bio.org" "/Users/pdcawley/Dropbox/org/from-mobile.org" "/Users/pdcawley/Dropbox/org/gtd.org" "/Users/pdcawley/Dropbox/org/headforwards.org" "/Users/pdcawley/Dropbox/org/house.org" "/Users/pdcawley/Dropbox/org/isolation-sessions.org" "/Users/pdcawley/Dropbox/org/journal.org" "/Users/pdcawley/Dropbox/org/kickstarting.org" "/Users/pdcawley/Dropbox/org/loafery-daybook.org" "/Users/pdcawley/Dropbox/org/loafery.org" "/Users/pdcawley/Dropbox/org/lyrics.org" "/Users/pdcawley/Dropbox/org/module-rename.org" "/Users/pdcawley/Dropbox/org/monads-beat-exceptions.org" "/Users/pdcawley/Dropbox/org/music-marketing.org" "/Users/pdcawley/Dropbox/org/newsletter.org" "/Users/pdcawley/Dropbox/org/open-source-entertainment.org" "/Users/pdcawley/Dropbox/org/oscon.org" "/Users/pdcawley/Dropbox/org/pay-to-play.org" "/Users/pdcawley/Dropbox/org/perl-build.org" "/Users/pdcawley/Dropbox/org/perl-community-minutes.org" "/Users/pdcawley/Dropbox/org/production_stuff.org" "/Users/pdcawley/Dropbox/org/rates-and-rules.org" "/Users/pdcawley/Dropbox/org/seedy-malt-ingredients.org" "/Users/pdcawley/Dropbox/org/singingtogether.org" "/Users/pdcawley/Dropbox/org/speedcubing.org" "/Users/pdcawley/Dropbox/org/template-test.org" "/Users/pdcawley/Dropbox/org/todo.org" "/Users/pdcawley/Dropbox/org/trad-song-tues.org" "/Users/pdcawley/Dropbox/org/twitch.org" "/Users/pdcawley/Dropbox/org/waterdeep-dragon-heist.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
