(require 'skeleton)

(defvar *dss-skeleton-markers* nil
  "Markers for locations saved in skeleton-positions")

(add-hook 'skeleton-end-hook 'dss/skeleton-make-markers)

(defun dss/skeleton-make-markers ()
  (while *dss-skeleton-markers*
    (set-marker (pop *dss-skeleton-markers*) nil))
  (setq *dss-skeleton-markers*
        (mapcar 'copy-marker (reverse skeleton-positions))))

(defun dss/skeleton-next-position (&optional reverse)
  (interactive "P")
  (let ((positions (mapcar 'marker-position *dss-skeleton-markers*))
        (comp (if reverse '> '<))
        pos)
    (when positions
      (if (catch 'break
            (while (setq pos (pop positions))
              (when (funcall comp (point) pos)
                (throw 'break t))))
          (goto-char pos)
        (goto-char (marker-position
                    (if reverse
                        (car (last *dss-skeleton-markers*))
                      (car *dss-skeleton-markers*))))))))

(provide 'dss-skeleton)
