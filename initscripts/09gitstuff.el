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

(use-package magit
  :init
  (progn
    (defvar pdc/magit-map)
    (define-prefix-command 'pdc/magit-map)

    (bind-key "C-. g" 'pdc/magit-map)
    (bind-key "C-. g s" 'magit-status))
  :config
  (progn
    (defadvice magit-status (before magit-status-local-gitexec activate)
      "Apply dir locals before running magit"
      (hack-dir-local-variables-non-file-buffer))

    (defadvice magit-git-lines (before magit-git-lines-local-gitexec activate)
      (hack-dir-local-variables-non-file-buffer))

    (defadvice magit-git-string (before magit-git-string-local-gitexec
                                        activate)
      (hack-dir-local-variables-non-file-buffer))

    (defadvice magit-cmd-insert (before magit-cmd-insert-local-gitexec
                                        activate)
      (hack-dir-local-variables-non-file-buffer))
    ))

(require 'vc-git)
(defun pdc/bookmark-magit-status (bookmark)
  "Run magit-status on the bookmarked file"
  (interactive (list (bookmark-completing-read "Status of bookmark" (bmkp-default-bookmark-name))))
  (magit-status (bookmark-prop-get bookmark 'filename)))

(defun vc-git-annotate-command (file buff &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "-w" "--date-iso" "-C" "-C" rev "--" name)))
