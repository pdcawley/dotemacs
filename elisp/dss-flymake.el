(require 'flymake)

(defvar dss-flymake-file-init-func-hash (make-hash-table))
(defvar dss-flymake-file-cleanup-func-hash (make-hash-table))

(defun flymake-get-init-function (file-name)
  "Return init function to be used for the file."
  (let* ((init-f  (nth 0 (flymake-get-file-name-mode-and-masks file-name))))
    ;;(flymake-log 0 "calling %s" init-f)
    ;;(funcall init-f (current-buffer))
    (or (gethash file-name dss-flymake-file-init-func-hash) init-f)))

(defun flymake-get-cleanup-function (file-name)
  "Return cleanup function to be used for the file."
  (or (gethash file-name dss-flymake-file-cleanup-func-hash)
      (nth 1 (flymake-get-file-name-mode-and-masks file-name))
      'flymake-simple-cleanup))

(defun dss/flymake-ignore ()
  (interactive)
  (save-excursion
    (call-interactively 'comment-dwim)
    (end-of-line)
    (insert " flymake-ignore")))


(defun dss/flymake-filter-line-err (n)
  (interactive "nLine: ")
  (or (loop for ln in flymake-err-info
            when (not (eq (car ln) n))
            collect ln)
      '()))
(defvar dss-flymake-output-filtered nil)

(defadvice flymake-parse-output-and-residual (before flymake-parse-output-and-residual-before-dss
                                                     activate)
  (setq dss-flymake-output-filtered nil)
  (flymake-report-status "" "")
                                        ;ad-do-it
  )
(defvar dss-flymake-filtering-on t)
(defun dss/flymake-toggle-filter ()
  (interactive)
  (if dss-flymake-filtering-on
      (setq dss-flymake-filtering-on nil)
    (setq dss-flymake-filtering-on t))
  (flymake-start-syntax-check))

(defadvice flymake-highlight-line (around flymake-highlight-line-around-dss
                                          activate)
  "Ignore lines with flymake-ignore comment"
  (let ((line-no (ad-get-arg 0))
        (line-err-info-list (ad-get-arg 1)))
    (goto-char (point-min))
    (forward-line (1- line-no))
    (if (or (not dss-flymake-filtering-on)
            (not (looking-at ".*flymake-ignore.*$")))
        ad-do-it
      (progn
        (setq dss-flymake-output-filtered t)
        (setq flymake-check-was-interrupted t)
        (setq flymake-err-info (dss/flymake-filter-line-err line-no))))))

;; (defadvice flymake-post-syntax-check (around flymake-post-syntax-check-around-dss activate)
;;   ;; (exit-status command)
;;   (if (and dss-flymake-output-filtered
;;            (not flymake-err-info))
;;       (ad-set-arg 0 0))
;;   ad-do-it)

;; (defun flymake-post-syntax-check (exit-status command)
;;   (setq flymake-err-info flymake-new-err-info)
;;   (setq flymake-new-err-info nil)
;;   (setq flymake-err-info
;;         (flymake-fix-line-numbers
;;          flymake-err-info 1 (flymake-count-lines)))
;;   (flymake-delete-own-overlays)
;;   (flymake-highlight-err-lines flymake-err-info)
;;   (let (err-count warn-count)
;;     (setq err-count (flymake-get-err-count flymake-err-info "e"))
;;     (setq warn-count  (flymake-get-err-count flymake-err-info "w"))
;;     (flymake-log 2 "%s: %d error(s), %d warning(s) in %.2f second(s)"
;;                  (buffer-name) err-count warn-count
;;                  (- (flymake-float-time) flymake-check-start-time))
;;     (setq flymake-check-start-time nil)

;;     (if (and (equal 0 err-count) (equal 0 warn-count))
;;         (if (equal 0 exit-status)
;;             (flymake-report-status "" "")       ; PASSED
;;           (if (not flymake-check-was-interrupted)
;;               (flymake-report-fatal-status "CFGERR"
;;                                            (format "Configuration error has occurred while running %s" command))
;;             (flymake-report-status nil ""))) ; "STOPPED"
;;       (flymake-report-status (format "%d/%d" err-count warn-count) ""))))

;; (defun flymake-get-real-file-name-function (file-name)
;;   (or (nth 2 (flymake-get-file-name-mode-and-masks file-name))
;;       'flymake-get-real-file-name))

(provide 'dss-flymake)
