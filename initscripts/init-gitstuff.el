(eval-when-compile (require 'req-package))

(require 'eieio)
(req-package with-editor
  :diminish "")

(req-package exec-path-from-shell)

(req-package magit
  :requires (exec-path-from-shell)
  :general
  ("M-," 'pdc/vc-status)
  ("C-. g s" 'magit-status)
  (:prefix leader-key
   "g"   '(:ignore t :which-key "git")
   "g s" 'magit-status)
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-branch-arguments nil)
  (setq magit-push-always-verify nil)
  :config
  (defun pdc/bookmark-magit-status (bookmark)
    "Run magit-status on the bookmarked file"
    (interactive
     (list (bookmark-completing-read "Status of bookmark"
                                     (bmkp-default-bookmark-name))))
    (magit-status (bookmark-prop-get bookmark 'filename)))
  (defun pdc/vc-status ()
    (interactive)
    (cond ((magit-get-top-dir default-directory)
           (call-interactively 'magit-status))
          (t
           (call-interactively 'dired)))))

(req-package magit-gitflow
  :require magit
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(req-package gist
  :ensure t
  :init (setq gist-authenticate-function 'gist-oauth2-authentication)
  :bind ("C-c G" . gist-region-or-buffer))

(req-package git-blame
  :ensure t
  :commands git-blame-mode)

(req-package git-gutter+
  :ensure t
  :if (window-system)
  :diminish git-gutter+-mode
  :config
  (progn
    (req-package git-gutter-fringe+
      :ensure t
      :config
      (git-gutter-fr+-minimal)))

  (global-git-gutter+-mode 1))

(req-package vc-git
  :init
  (setq vc-git-diff-switches "-b")
  :config
  (progn
    (defun vc-git-annotate-command (file buff &optional rev)
      (let ((name (file-relative-name file)))
        (vc-git-command buff 'async nil "blame" "-w" "--date-iso" "-C" "-C"
  rev "--" name)))))

(req-package git-timemachine)
