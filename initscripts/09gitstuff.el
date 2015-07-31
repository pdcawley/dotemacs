(require 'eieio)
(use-package magit
  :bind (("M-," . pdc/vc-status)
         ("C-. g s" . magit-status))
  :init
  (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
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
             (call-interactively 'dired))))
    (use-package magit-gitflow
      :config
      (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))))



(use-package gist
  :init (setq gist-authenticate-function 'gist-oauth2-authentication)
  :bind ("C-c G" . gist-region-or-buffer))

(use-package git-blame
  :commands git-blame-mode)

(use-package git-gutter+
  :diminish git-gutter+-mode
  :config
  (progn
    (use-package git-gutter-fringe+
      :config
      (git-gutter-fr+-minimal))

    (global-git-gutter+-mode 1)))

(use-package vc-git
  :init
  (setq vc-git-diff-switches "-b")
  :config
  (progn
    (defun vc-git-annotate-command (file buff &optional rev)
      (let ((name (file-relative-name file)))
        (vc-git-command buff 'async nil "blame" "-w" "--date-iso" "-C" "-C" rev "--" name)))))
