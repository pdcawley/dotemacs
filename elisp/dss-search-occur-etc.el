(setq-default case-fold-search t)

;; (require 'isearch+)

(eval-after-load "isearch" '(require 'isearch+))
;;isearch-message-prefix

;; follow mode for output of grep and occur
;;; (require 'fm)
;;; (defun fm-start-off()
;;;   (fm-start)
;;;   (fm-toggle))
;;; (add-hook 'occur-mode-hook 'fm-start-off)
;;; ;(add-hook 'ibuffer-mode-hooks 'fm-start-off)
;;; (add-hook 'ibuffer-occur-mode-hooks 'fm-start-off)
;;;

;;; isearch-forward-at-point
;; from http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))


;;; http://www.emacswiki.org/emacs/KillISearchMatch
  ;; (defun kill-isearch-match ()
  ;;   "Kill the current isearch match string and continue searching."
  ;;   (interactive)
  ;;   (kill-region isearch-other-end (point)))
  ;; (define-key isearch-mode-map [(control k)] 'kill-isearch-match)

  ;; Comment line matching and continue searching:
  ;; (define-key isearch-mode-map (kbd "C-c")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (save-excursion
  ;;       (comment-region (point-at-bol) (point-at-eol)))
  ;;     (if isearch-forward
  ;;         (isearch-repeat-forward)
  ;;         (isearch-repeat-backward))))

  ;; ;; Kill line matching and continue searching:
  ;; (define-key isearch-mode-map (kbd "C-k")
  ;;   (lambda ()
  ;;     (interactive)
  ;;     (save-excursion
  ;;       (beginning-of-line)
  ;;       (kill-line))
  ;;     (if isearch-forward
  ;;         (isearch-repeat-forward)
  ;;         (isearch-repeat-backward))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/
(defun dss/get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (if (eq mode major-mode)
            (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun dss/multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
      (car (occur-read-primary-args))))

(require 'inline-string-rectangle)
(require 'mark-more-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-search-occur-etc)
