;;; setting up clojure/slime http://technomancy.us/126
(require 'dss-slime)
(require 'clojure-mode)
(require 'cljdoc)

(defmacro dss/defface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(dss/defface dss-clojure-punctuation "#afaf5f" "Clojure punctuation")
(dss/defface dss-clojure-braces "#49b2c7" "Clojure braces")
(dss/defface dss-clojure-brackets "#49b2c7" "Clojure brackets")
(dss/defface dss-clojure-brackets-highlight "#49b2c7"
             "Clojure brackets highlight" (:background "#333333"))
(dss/defface dss-clojure-keyword "khaki" "Clojure keywords")
(dss/defface dss-clojure-namespace "#c476f1" "Clojure namespace")
(dss/defface dss-clojure-quote "white" "Clojure quote" (:background "#333333"))
(dss/defface dss-clojure-backtick "brightwhite" "Clojure backtick"
             (:background "#5f0000"))
(dss/defface dss-clojure-java-call "#5f87ff" "Clojure Java calls")
(dss/defface dss-clojure-strf "bright green" "Clojure strf" (:background "#333333"))
(dss/defface dss-clojure-number "#b8bb00" "Clojure number")
(dss/defface dss-clojure-backslash "magenta" "Clojure backslash")

(dss/defface dss-clojure-function-arity
             "khaki" "Clojure function arity"
             (:background "#262626"))

(dss/defface dss-clojure-anon-fn-macro "magenta" "Clojure #() fn"
             (:background "#262626"))

(dss/defface dss-clojure-regex "magenta" "Clojure regex"
             (:background "#262626"))
(dss/defface dss-clojure-ignore-form "unspecified"
             "clojure (comment) or #_ macro"
             (:background "#262626"))

(defun dss/clojure-match-ignore-form (&optional limit)
  (interactive)
  (if (search-forward-regexp "\\(#_\\|(comment\\)" limit 'no-error)
      (progn
        (save-match-data
          (if (looking-back "comment" (- (point) 7))
              (dss/out-one-sexp)))
        (set-match-data (list (car (match-data)) (scan-sexps (point) 1)))
        (add-text-properties (car (match-data))
                             (second (match-data)) '(font-lock-multiline t))
        t)))

(defun dss/test-clojure-fontlock ()
  (interactive)
  (save-excursion
    (goto-line 60)
    (call-interactively 'eval-defun))
  (save-window-excursion
    (with-current-buffer "html.clj"
      (clojure-mode))))

(defun dss/clojure-add-extra-fontlock ()
  (interactive)
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          `(
            ((dss/clojure-match-cond 0 'font-lock-keyword-face append))
            ((dss/clojure-match-ignore-form
              0 'dss-clojure-ignore-form append))
            (("\\\\" . 'dss-clojure-backslash))
            (("\\(|\\|=\\|,\\|&\\|~\\|@\\|#\\|+\\|_\\|:\\)"
              . 'dss-clojure-punctuation))
            (("#?\\^?{\\|}" . 'dss-clojure-braces))
            (("\\[\\|\\]" . 'dss-clojure-brackets))
            (("'" . 'dss-clojure-quote))
            (("`\\|@\\|~" . 'dss-clojure-backtick))
            (("[^\\w+]\\(:\\w+\\)" 1 'dss-clojure-keyword))

            ((dss/clojure-match-anan-fn-macro
              (1 'dss-clojure-anon-fn-macro)
              (2 'dss-clojure-anon-fn-macro)))
            (("%[0-9.]*\\(([a-zA-Z0-9]*)\\)?[fdsfr]*" 0 'dss-clojure-strf prepend))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)"
              1 'dss-clojure-java-call))
            (("\\(^\\|[ \\[({]\\)\\([0-9]+\\)" 2 'dss-clojure-number append))
            (("#\"" 0 'dss-clojure-regex append))

            ((dss/clojure-match-function-arity
              1 'dss-clojure-function-arity prepend))

            ((dss/clojure-match-core-match 0 'font-lock-keyword-face append))

            ((dss/clojure-match-for-comps
              (2 'dss-clojure-brackets-highlight)
              (3 'dss-clojure-brackets-highlight)
              ))
            ))
  (font-lock-fontify-buffer))

;; see https://github.com/purcell/emacs.d/blob/master/init-clojure.el
(defun dss/add-end-of-sexp-to-match-data (sexp-start-group)
  (let ((end (scan-sexps (match-beginning sexp-start-group) 1)))
    (set-match-data (append
                     (list (match-beginning 0))
                     (list end)
                     (cdr (cdr (match-data)))
                     (list (1- end) end)))
    (add-text-properties (match-beginning 0) end '(font-lock-multiline t))
    t))

(defun dss/clojure-match-anan-fn-macro (&optional limit)
  (interactive)
  (if (and (search-forward-regexp "\\(\\#(\\|(fn\\)" limit 'no-error)
           (not (save-match-data (dss/in-string-p))))
      (dss/add-end-of-sexp-to-match-data 1)))

(defun dss/clojure-match-for-comps (&optional limit)
  (interactive)
  (if (and
       (search-forward-regexp "\\(for\\|doseq\\) *\\(\\[\\)" limit 'no-error)
       (not (save-match-data (dss/in-string-p))))
      (dss/add-end-of-sexp-to-match-data 2)))

(defun dss/clojure-match-function-arity (&optional limit)
  (interactive)
  (if (and
       (search-forward-regexp "  \\((\\)\\[" limit 'no-error)
       (not (save-match-data (dss/in-string-p))))
      (save-match-data
        (save-excursion
          (goto-char (match-beginning 0))
          (= (syntax-ppss-depth (syntax-ppss)) 1)))))

;; (defun dss/clojure-match-core-match (&optional limit)
;;   (interactive)
;;   (if (search-forward-regexp "^[ ]+\\(\\[\\)" limit 'no-error)
;;       (if (and (save-excursion
;;                  (save-match-data
;;                    (backward-char)
;;                    (backward-up-list))
;;                  (looking-at-p "(match"))
;;                t
;;                ;; (not (save-match-data
;;                ;;        (save-excursion
;;                ;;          (forward-line -1)
;;                ;;          (back-to-indentation)
;;                ;;          (looking-at-p "\\["))))
;;                )
;;           (dss/add-end-of-sexp-to-match-data 1))))

(defun dss/clojure-match-core-match (&optional limit)
  (interactive)
  (if (and (search-forward-regexp "\\((match \\|(match/match \\)" limit 'no-error)
           (not (save-match-data (dss/in-string-p))))
      (let* ((beg (match-beginning 0))
             (end (scan-sexps beg 1)))
        (save-excursion
          (goto-char beg)
          (dolist (pair (partition (cdr (cdr (dss/lisp-get-child-sexp-markers))) 2))
            (dss/font-lock-propertize-char (car pair) 'dss-clojure-backtick)
            (dss/font-lock-propertize-char
             (1- (scan-sexps (1- (car pair)) 1)) 'dss-clojure-backtick))
          (add-text-properties beg end '(font-lock-multiline t)))
        t)))
;;; ;;;;
(defun dss/clojure-match-cond (&optional limit)
  (interactive)
  (if (and (search-forward-regexp "\\((cond$\\|(cond \\)" limit 'no-error)
           (not (save-match-data (dss/in-string-p))))
      (let* ((beg (match-beginning 0))
             (end (scan-sexps beg 1)))
        (save-excursion
          (goto-char beg)
          (dolist (pair (partition (cdr (dss/lisp-get-child-sexp-markers)) 2))
            (dss/font-lock-propertize-char (car pair) 'dss-clojure-function-arity)
            (dss/font-lock-propertize-char
             (1- (scan-sexps (1- (car pair)) 1)) 'dss-clojure-function-arity))
          (add-text-properties beg end '(font-lock-multiline t)))
        t)))

;; inlining this function for speed:
;; see: http://www.gnu.org/s/emacs/manual/html_node/elisp/Compilation-Tips.html
;; this will cause problems with debugging. To debug, change defsubst -> defun.
(defun dss/font-lock-propertize-char (point face)
  (with-silent-modifications
    (add-text-properties point (1+ point)
                         `(font-lock-face ,face rear-nonsticky t))))

(defun dss/font-lock-unpropertize-char (point)
  "Remove text properties from char at POINT."
  (with-silent-modifications
    (remove-text-properties point (1+ point)
                            '(font-lock-face nil rear-nonsticky nil))))

(defun dss/font-lock-unpropertize-region (start end)
  "Remove mode faces from chars in region between START and END."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (progn
        (dss/font-lock-unpropertize-char (point))
        (forward-char)))))

(defun dss/clojure-flash-sexps ()
  (interactive)
  (let* ((beg (save-excursion (backward-up-list) (point)))
         (end (scan-sexps beg 1)))
    (save-excursion
      (goto-char beg)
      (dolist (pair (partition (cdr (dss/lisp-get-child-sexp-markers)) 2))
        (dss/font-lock-propertize-char (car pair) 'dss-clojure-backtick)))
    (save-excursion (font-lock-fontify-region beg end))
    (sit-for 2)
    (save-excursion
      (dss/font-lock-unpropertize-region beg end)
      (font-lock-fontify-region beg end))))

(defun dss/lisp-get-child-sexp-markers ()
  (save-excursion
    (let ((start (1+ (point))))
      (forward-sexp)
      (backward-char)
      (reverse (loop while (> (point) start)
                     do (backward-sexp)
                     collect (point-marker))))))

;; (defun dss/clojure-match-core-match (&optional limit)
;;   (interactive)
;;   (if (search-forward-regexp "^[ ]+\\(\\[\\)" limit 'no-error)
;;       (and
;;        (save-excursion
;;          (save-match-data
;;            (backward-char)
;;            (backward-up-list))
;;          (add-text-properties
;;           (point) (match-end 0) '(font-lock-multiline t))
;;          (looking-at-p "(match"))
;;        (not (save-excursion
;;               (previous-line)
;;               (back-to-indentation)
;;               (looking-at-p "\\["))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dss/clojure-ignore-form ()
  "Inserts, toggles, or moves Clojure's #_ ignore-next-form reader macro."
  (interactive)
  (flet ((in-string-p () (eq 'string (syntax-ppss-context (syntax-ppss))))
         (in-comment-p () (eq 'comment (syntax-ppss-context (syntax-ppss)))))
    (skip-chars-forward " ")
    (while (in-string-p)
      (backward-char))
    (cond
     ;; switch from a comment to an ignore form, if paredit is enabled
     ;; This bit might not work if mark-sexp fails inside the comment.
     ((and (featurep 'paredit)
           (or (looking-at-p ";") (in-comment-p))
           (looking-at-p "[ ;]*\("))
      (progn
        (back-to-indentation)
        (mark-sexp)
        (paredit-comment-dwim)
        (insert "#_")))

     ;; if the command is repeated, move the ignore macro up a syntax level
     ((and (equal last-command this-command)
           (> (syntax-ppss-depth (syntax-ppss)) 0)
           (looking-back "#_ *" (- (point) 5)))
      (progn
        (replace-match "")
        (backward-up-list)
        (insert "#_")))

     ;; if the cursor is right on top of the ignore macro, remove it
     ((looking-at-p "#_")
      (delete-char 2))
     ;; ditto, if in the middle of it
     ((and (looking-at-p "_")
           (looking-back "#"))
      (progn (backward-char)
             (delete-char 2)))

     ;; if an outer form is already ignored, un-ignore (sic) it
     ((save-excursion
        (while (and (> (syntax-ppss-depth (syntax-ppss)) 0)
                    (not (looking-back "#_ *" (- (point) 10))))
          (backward-up-list))
        (looking-back "#_ *" (- (point) 10)))
      (save-excursion (replace-match "")))
     ;; else, just ignore this form
     (t (insert "#_")))))

(defun dss/slime-repl-after-pmark-p ()
  (>= (point) slime-repl-input-start-mark))

(defmacro dss/def-alternate-key (name pred match no-match)
  `(defun ,name ()
     (interactive)
     (if (,pred)
         (call-interactively ,match)
       (call-interactively ,no-match))))

(dss/def-alternate-key dss/slime-repl-next-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-next-input 'next-line)

(dss/def-alternate-key dss/slime-repl-previous-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-previous-input 'previous-line)

(dss/def-alternate-key dss/slime-repl-previous-matching-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-previous-matching-input 'scroll-down-command)

(dss/def-alternate-key dss/slime-repl-next-matching-input
                       dss/slime-repl-after-pmark-p
                       'slime-repl-next-matching-input 'scroll-up-command)

(define-skeleton dss/clojure-use-skeleton
  "use skeleton"
  nil
  "(use '" @ - _ ")")

(dss/def-alternate-key dss/slime-repl-use
                       slime-repl-at-prompt-start-p
                       'dss/clojure-use-skeleton 'self-insert-command)



(define-skeleton dss/clojure-defun-skeleton
  "clojure defun skeleton"
  nil
  "(defn " @ - " [" @ "]" \n >
  @ _
    ")")

(define-skeleton dss/clojure-let-skeleton
  "let skeleton"
  nil
  "(let [" @ - "]" \n >
  @ _ ")")


(define-skeleton dss/clojure-do-skeleton
  "A simple e-lisp progn skeleton"
  nil
  "(do" @ - \n >
  @ _ ")"
    (dss/indent-defun)
    (back-to-indentation))
(defun dss/clojure-setup-skeletons ()
  (interactive)
  (set (make-local-variable 'dss-progn-skeleton-func)
       'dss/clojure-do-skeleton)
  (set (make-local-variable 'dss-defun-skeleton-func)
       'dss/clojure-defun-skeleton)
  (set (make-local-variable 'dss-let-skeleton-func)
       'dss/clojure-let-skeleton))

(defun dss/clojure-mode-hook ()
  (interactive)
  (define-key clojure-mode-map (kbd "M-'") 'dss/clojure-ignore-form)
  (define-key clojure-mode-map (kbd "C-M-w") 'dss/indent-sexp)

  ;; (mapc (lambda (k)
  ;;         (define-key clojure-mode-map k 'dss/clojure-load-current-file))
  ;;       (list (kbd "C-x C-s") [(f12)]))
  (dss/install-whitespace-cleanup-hook)
  (turn-on-auto-fill)
  (dss/load-lineker-mode)
  (dss/highlight-watchwords)
  (setq mode-name "clj")
  (dss/clojure-add-extra-fontlock)
  (dss/clojure-setup-skeletons))

(add-hook 'clojure-mode-hook 'dss/clojure-mode-hook)


(define-clojure-indent
  (match 1)
  (match/match 1)

  ;; crux indentation
  (entity 'defun)
  (events 'defun)
  (defdomain 'defun))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq clojure-test-ns-segment-position 1)
(setq clojure-swank-command "~/bin/lein jack-in %s")
(defun dss/clojure-jack-in ()
  (interactive)
  (clojure-jack-in))

(setq slime-auto-connect 'always)
(defun slime (&optional command coding-system)
  "Start a lein swank server and connect to it."
  (interactive)
  (clojure-jack-in))

(defun dss/clojure-repl-switch-to-current-ns ()
  (interactive)
  (save-buffer)
  (slime-compile-and-load-file)
  (let ((package (clojure-find-ns)))
    (with-current-buffer (slime-output-buffer)
      (let ((previouse-point (- (point) slime-repl-input-start-mark)))
        (destructuring-bind (name prompt-string)
            (slime-repl-shortcut-eval `(swank:set-package ,package))
          (setf (slime-lisp-package) name)
          (setf (slime-lisp-package-prompt-string) prompt-string)
          (setf slime-buffer-package name)
          (slime-repl-insert-prompt)
          (when (plusp previouse-point)
            (goto-char (+ previouse-point slime-repl-input-start-mark)))))))
  ;; (slime-repl-set-package (clojure-find-ns))
  )

;; (import (java.io File)
;;         (java.net URL URLClassLoader)
;;         (java.lang.reflect Method))
;; (defn add-to-cp [#^String jarpath]
;;   (let [#^URL url (.. (File. jarpath) toURI toURL)
;;         url-ldr-cls (. (URLClassLoader. (into-array URL [])) getClass)
;;         arr-cls (into-array Class [(. url getClass)])
;;         arr-obj (into-array Object [url])
;;         #^Method mthd (. url-ldr-cls getDeclaredMethod "addURL" arr-cls)]
;;     (doto mthd
;;       (.setAccessible true)
;;       (.invoke (ClassLoader/getSystemClassLoader) arr-obj))
;;     (println (format "Added %s to classpath" jarpath))))

;; (defun dss/clojure-reset-namespace ()
;;   (interactive)
;;   (slime-interactive-eval (format
;;                            "
;; (ns dss-util)
;; (defn ns-clean
;;   ([] (ns-clean *ns*))
;;   ([ns] (map (fn [v] (ns-unmap ns v)) (keys (ns-interns ns)))))

;; (in-ns %s)
;; (require 'dss-util)
;; (dss-util/ns-clean)
;; " (clojure-find-ns))))

(defun dss/clojure-list-namespace ()
  (interactive)
  (slime-interactive-eval "(keys (ns-interns *ns*))"))

(defun dss/clojure-reset-namespace ()
  "Reloads the current Clojure namespace by first removing it and
then re-evaluating the slime buffer. Use this to remove old
symbol definitions and reset the namespace to contain only what
is defined in your current Emacs buffer.

"
  (interactive)
  (slime-interactive-eval "(map (fn [v] (ns-unmap *ns* v)) (keys (ns-interns *ns*)))")

  ;; (slime-eval-buffer)
  )

(defun dss/clojure-in-tests ()
  (string-match "test" (clojure-find-ns)))

(defun dss/clojure-jump-between-tests-and-code ()
  (interactive)
  (if (dss/clojure-in-tests)
      (clojure-test-jump-to-implementation)
    (clojure-jump-to-test)))

(defun dss/clojure-run-tests ()
  (interactive)
  (save-window-excursion
    (if (not (dss/clojure-in-tests))
        (clojure-jump-to-test))
    (clojure-test-run-tests)))

(defun dss/clojure-load-current-file ()
  (interactive)
  (save-buffer)
  (if (not (string-match-p
            "project\\.clj"
            (buffer-file-name)))
      (slime-compile-and-load-file)))

(defun dss/clojure-init-debugger ()
  (interactive)
  (slime-repl-eval-string "(require 'swank.cdt)"))

(defun dss/clojure-print-breakpoints ()
  (interactive)
  (slime-repl-eval-string "(require 'swank.cdt) (swank.cdt/print-bps)"))


(defun dss/clojure-set-breakpoint ()
  (interactive)
  (let ((ns (clojure-find-ns))
        (func (which-function)))
    (slime-repl-eval-string
     (format "(require 'swank.cdt) (swank.cdt/set-bp %s/%s)" ns func))))

(defun dss/clojure-clear-breakpoints ()
  (interactive)
  (slime-repl-eval-string "(require 'swank.cdt) (swank.cdt/delete-all-breakpoints)"))

(defun dss/slime-repl-clear ()
  (interactive)
  (save-window-excursion
    (slime-switch-to-output-buffer)
    (slime-repl-clear-buffer)
    (end-of-buffer)
    (dss/sync-point-all-windows)))

(defun dss/clojure-jump-to-project ()
  "Jump to project.clj"
  (interactive)
  (find-file (format "%s/project.clj"
                     (locate-dominating-file buffer-file-name "src/"))))

(defun dss/slime-repl-go ()
  (interactive)
  (if (one-window-p)
      (progn
        (split-window-horizontally)
        (call-interactively 'other-window)
        (switch-to-buffer (slime-output-buffer))
        (goto-char (point-max)))
    (slime-switch-to-output-buffer)))

(defun dss/slime-repl-mode-setup-map (&optional mode-map)
  (interactive)
  (let ((mode-map (or mode-map slime-repl-mode-map)))
    (define-key mode-map (kbd "<down>") 'dss/slime-repl-next-input)
    (define-key mode-map (kbd "<up>") 'dss/slime-repl-previous-input)
    (define-key mode-map (kbd "<prior>") 'dss/slime-repl-previous-matching-input)
    (define-key mode-map (kbd "<next>") 'dss/slime-repl-next-matching-input)

    ;; (define-key mode-map (kbd "C-M-r") 'comint-history-isearch-backward)
    ;; (define-key mode-map (kbd "C-M-s") 'comint-history-isearch-search)
    (define-key mode-map (kbd "M-r") 'paredit-raise-sexp)

    (define-key mode-map (kbd "M-p") 'previous-line)
    (define-key mode-map (kbd "M-n") 'next-line)
    (define-key mode-map (kbd "C-M-l") 'end-of-buffer)
    (define-key mode-map "{" 'paredit-open-curly)
    (define-key mode-map "}" 'paredit-close-curly)
    (define-key mode-map (kbd "DEL") 'dss/paredit-backward-delete)

    ;; (define-key mode-map "u" 'dss/slime-repl-use)
    ))

;;; see Clementson's Blog: Clojure SLIME Mods for Java Documentation
;;; http://bc.tech.coop/blog/081120.html
(setq slime-enable-evaluate-in-emacs t)

(defun dss/slime-repl-hook ()
  (interactive)

  ;; also see
  ;; https://github.com/technomancy/durendal/blob/master/durendal.el
  ;; and https://github.com/tcrayford/clojure-refactoring
  (paredit-mode 1)
  (dss/load-slime-completion)
  (dss/slime-repl-mode-setup-map)
  (dss/clojure-setup-skeletons)

  (when (< emacs-major-version 24)
    (set (make-local-variable 'forward-sexp-function)
         'clojure-forward-sexp))
  ;; the rest is semi-copied from
  ;; http://stackoverflow.com/questions/2474804/is-there-a-colored-repl-for-clojure
  (set (make-local-variable 'lisp-indent-function)
       'clojure-indent-function)
  (set (make-local-variable 'lisp-doc-string-elt-property)
       'clojure-doc-string-elt)
  (font-lock-mode nil)
  (clojure-mode-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (font-lock-mode t)

  ;; (ad-activate #'slime-repl-emit)
  ;; (ad-activate #'slime-repl-insert-prompt)
  (set (make-local-variable 'comment-start-skip)
       "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (dss/clojure-add-extra-fontlock)
  (eldoc-mode 1))
(add-hook 'slime-repl-mode-hook 'dss/slime-repl-hook)

(provide 'dss-clojure)
