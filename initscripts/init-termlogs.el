(defun cleanup-term-log ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max))
  (goto-char (point-min))
  (while (re-search-forward "\\(.\\|
$\\|P.+\\\\\n\\)" nil t)
    (overlay-put (make-overlay (match-beginning 0) (match-end 0))
                 'invisible t))
  (set-buffer-modified-p nil))

(add-hook 'find-file-hooks
          (function
           (lambda ()
             (if (string-match "/\\.iTerm/.*\\.log\\'"
                               (buffer-file-name))
                 (cleanup-term-log)))))
