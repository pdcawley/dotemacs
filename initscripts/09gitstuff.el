(require 'eieio)
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

(require 'magit)
(require 'vc-git)
(defun pdc/bookmark-magit-status (bookmark)
  "Run magit-status on the bookmarked file"
  (interactive (list (bookmark-completing-read "Status of bookmark" (bmkp-default-bookmark-name))))
  (magit-status (bookmark-prop-get bookmark 'filename)))

(defun vc-git-annotate-command (file buff &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "-w" "--date-iso" "-C" "-C" rev "--" name)))
