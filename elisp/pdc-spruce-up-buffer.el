(defcustom pdc|spruce-up-buffer-hook nil
  "Normal hook run after basic buffer spruce up commands"
  :type 'hook
  :group 'pdc)

(defun pdc|spruce-up-buffer ()
  "Perform a general cleanup of the current buffer"
  (interactive)
  (indent-region (point-min) (point-max))
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (run-hooks pdc|spruce-up-buffer-hook))

(provide 'pdc-spruce-up-buffer)
