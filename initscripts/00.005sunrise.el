;; (require 'sunrise-commander)
;; (require 'sunrise-x-checkpoints)
;; ;; (require 'sunrise-x-modeline)

;; (defun ido-sunrise ()
;;   "Call `sunrise' the ido way.
;;     The directory is selected interactively by typing a substring.
;;     For details on keybindings, see `ido-find-file'."
;;   (interactive)
;;   (let ((ido-report-no-match nil)
;;         (ido-auto-merge-work-directories-length -1))
;;     (ido-file-internal 'sr-dired 'sr-dired nil "Sunrise: "
;;                        'dir)))
;; ;;;(define-key (cdr (assoc 'ido-mode minor-mode-map-alist))
;; ;;;  [remap dired] 'ido-sunrise)
;; (setq find-directory-functions (cons 'sr-dired
;;                                      find-directory-functions))

;; (define-key sr-mode-map "=" 'dss/term-cd)
;; (define-key sr-mode-map "." 'dss/term-select-window)
(provide 'dss-sunrise)
