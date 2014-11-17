(require 'dss-paths)
(require 'smex)

;; (dss/smex "^dss/moz" "")
(defun dss/smex (&optional filter initial-input)
  (interactive)
  (let* ((filter (or filter "^dss/"))
         (initial-input (or initial-input "dss/")))
    (smex-read-and-run
     (remq nil (mapcar (lambda (symbol-name)
                         (if (string-match-p filter symbol-name)
                             symbol-name
                           nil))
                       smex-ido-cache))
     initial-input)))

(setq smex-save-file (concat dss-ephemeral-dir "smex.save"))
(setq smex-history-length 10)
(setq smex-flex-matching t)
(smex-initialize)
(provide 'dss-smex)
