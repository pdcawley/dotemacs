(require 'eieio)
(require 'gist)
(setq gist-authenticate-function 'gist-oauth2-authentication)
(require 'magit)
(require 'vc-git)
(defun pdc/bookmark-magit-status (bookmark)
  "Run magit-status on the bookmarked file"
  (interactive (list (bookmark-completing-read "Status of bookmark" (bmkp-default-bookmark-name))))
  (magit-status (bookmark-prop-get bookmark 'filename)))

(defun vc-git-annotate-command (file buff &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "-w" "--date-iso" "-C" "-C" rev "--" name)))
