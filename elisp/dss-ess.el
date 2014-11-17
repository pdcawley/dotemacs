(require 'dss-elisp-funcs)

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/ess")

(condition-case nil
    (require 'ess-site)
  (message "Error while loading ess"))

(defun dss/ess-end-of-buffer ()
  (with-current-buffer "*R*"
    (end-of-buffer)
    (dss/sync-point-all-windows)))

(defun dss/ess-load-file ()
  (interactive)
  (call-interactively 'ess-eval-buffer)
  (dss/ess-end-of-buffer)
  (run-with-timer 1 nil 'dss/ess-end-of-buffer))

(defun dss/ess-eval-paragraph-and-step ()
  (interactive)
  (mark-paragraph)
  (setq mark-active nil)
  (call-interactively 'dss/flash-region)
  (call-interactively 'ess-eval-paragraph-and-step))

(defun dss/ess-eval ()
  (interactive)
  (if mark-active
      (call-interactively 'ess-eval-region-and-go)
    (call-interactively 'dss/ess-eval-paragraph-and-step))
  (run-with-timer 0.3 nil 'dss/ess-end-of-buffer)
  (run-with-timer 1 nil 'dss/ess-end-of-buffer))

(defun dss/ess-tag-it ()
  (interactive)
  (save-excursion
    (k2-copy-whole-sexp)
    (with-current-buffer "*R*"
      (insert "it <- ")
      (call-interactively 'yank)
      (comint-send-input ""))))

(defun dss/ess-electric-pair ()
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defvar *dss-ess-normal-underscore* nil)

(defun dss/ess-toggle-normal-underscore ()
  (interactive)
  (make-local-variable '*dss-ess-normal-underscore*)
  (if *dss-ess-normal-underscore*
      (progn
        (setq *dss-ess-normal-underscore* nil)
        (message "smart underscore"))
    (progn
      (setq *dss-ess-normal-underscore* t)
      (message "normal underscore"))))

(defun dss/ess-smart-underscore ()
  (interactive)
  (if (or *dss-ess-normal-underscore*
          (looking-back "[:upper:]" 1))
      (insert "_")
    (ess-smart-underscore)))

(defun dss/ess-ac-setup ()
  (interactive)
  (require 'ac-R)
  (make-local-variable 'ac-ignore-case)
  (make-local-variable 'ess-use-R-completion)
  (setq ess-use-R-completion nil)
  (setq ac-ignore-case t)
  (setq ac-sources
        '(ac-source-filename
          ac-source-words-in-same-mode-buffers
          ac-source-R
          ac-source-abbrev
          ac-source-dictionary)))

(defun dss/ess-mode-hook ()
  (interactive)
  (linum-mode 1)
  (dss/map-define-key ess-mode-map '("\"" "(" "[" "{" "'") 'dss/ess-electric-pair)

  (define-key ess-mode-map (kbd "C-c C-c") #'dss/ess-load-file)
  (define-key ess-mode-map "_" #'dss/ess-smart-underscore)
  (dss/ess-ac-setup))

(add-hook 'ess-mode-hook 'dss/ess-mode-hook)

(defun dss/ess-help-mode-hook ()
  (interactive)
  (linum-mode 1)
  (define-key ess-help-mode-map "q" #'dss/kill-buffer))

(add-hook 'ess-help-mode-hook 'dss/ess-help-mode-hook)

(require 'ess-inf)
(defun dss/inferior-ess-mode-hook ()
  (interactive)
  ;; (linum-mode 1)
  (define-key ess-mode-map "_" #'dss/ess-smart-underscore)
  (dss/ess-ac-setup)
  ;;(define-key ess-help-mode-map (kbd "M-.") #'ess-help)
  (dss/map-define-key inferior-ess-mode-map '("\"" "(" "[" "{") 'dss/ess-electric-pair))
(add-hook 'inferior-ess-mode-hook 'dss/inferior-ess-mode-hook)



(require 'skeleton)

(define-skeleton dss/ess-get-csv-skeleton
  "A simple read.csv skeleton"
  nil
  "df <- read.csv('./"
  _
  "', as.is=TRUE)\n")

(define-skeleton dss/ess-subset-skeleton
  "A subset function skeleton"
  nil
  "df <- subset(df," \n
  > "subset=(" \n
  > @ "TRUE" \n
  > ")," \n
  > "select=c(" @ _ "))")

(define-skeleton dss/ess-display-plot-skeleton
  "A display_plot skeleton"
  nil
  "display_plot(" @ _ ", filename=\"" @ - ".png\")")


;; (define-key inferior-ess-mode-map (kbd "C-d")
;;   (lambda ()
;;     (interactive)
;;     (ess-quit-r)))

(provide 'dss-ess)
