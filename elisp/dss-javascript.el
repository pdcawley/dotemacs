(require 'dss-paths)
(require 'cc-vars)
(require 'dss-codenav-helpers)

;;; http://weblogs.asp.net/george_v_reilly/archive/2009/03/24/exuberant-ctags-and-javascript.aspx
;;; http://tbaggery.com/2011/08/08/effortless-ctags-with-git.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Yegge's js2-mode with better indentation support

(add-to-list 'load-path (concat dss-vendor-dir "js2-mode"))
(require 'js2-mode)

(defun dss/js-electric-pair ()
  ;; this version doesn't check to see if we're inside of a string or comment
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/js2-indent-function ()
  "This is just a copy of http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode"
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

;; (require 'flymake-jslint)
(require 'flymake-node-jshint)

;;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/
(defun dss/js-ctags-here ()
  (interactive)
  (shell-command "ctags -e --recurse --extra=+fq --languages=javascript"))

(defun js2-mode-split-string (parse-status)
  "Turn a newline in mid-string into a string concatenation.
PARSE-STATUS is as documented in `parse-partial-sexp'."
  (let* ((col (current-column))
         (quote-char (nth 3 parse-status))
         (quote-string (string quote-char))
         (string-beg (nth 8 parse-status))
         (indent (save-match-data
                   (or
                    (save-excursion
                      (back-to-indentation)
                      (if (looking-at "\\+")
                          (current-column)))
                    (save-excursion
                      (goto-char string-beg)
                      (if (looking-back "\\+\\s-+")
                          (goto-char (match-beginning 0)))
                      (current-column))))))
    (insert quote-char " +\n")
    (indent-to indent)
    (insert quote-string)
    (when (eolp)
      (insert quote-string)
      (backward-char 1))))

(defun dss/js-insert-semicolon ()
  (interactive)
  (cond ((eq last-command this-command)
         (end-of-line))
        ((not (or (dss/in-string-p)
                  (dss/in-comment-p)))
         (save-excursion
           (end-of-line)
           (when (not (looking-back ";"))
             (insert ";"))
           ))
        (t (self-insert-command 1))))

(require 'jquery-doc)
(defun dss/js2-mode-hook ()
  (require 'espresso)
  (flymake-mode)
  ;; (setq ac-sources '(ac-source-semantic-raw))
  (setq mode-name "JS")
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-newline 0)
  (c-toggle-hungry-state 1)
  (setq js2-indent-on-enter-key t)
  (setq js2-enter-indents-newline t)
  (jquery-doc-setup)
  (set (make-local-variable 'indent-line-function) 'dss/js2-indent-function)
  (define-key js2-mode-map [(meta control "|")] 'cperl-lineup)
  (define-key js2-mode-map [(meta control "\;")]
    (lambda()
      (interactive)
      (insert "/* -----[ ")
      (save-excursion
        (insert " ]----- */"))
      ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'dss/indent-sexp)
  (define-key js2-mode-map ";" 'dss/js-insert-semicolon)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (mapc (lambda (char)
          (progn
            (define-key js2-mode-map char 'dss/js-electric-pair)))
        '("\"" "'" "(" "[" "{")))

(add-hook 'js2-mode-hook 'dss/js2-mode-hook)
(add-hook 'js2-mode-hook 'dss/install-whitespace-cleanup-hook)
(add-hook 'js2-mode-hook '(lambda () (linum-mode t)))

(defun dss/jslint-ignore ()
  (interactive)
  (save-excursion
    (call-interactively 'comment-dwim)
    (end-of-line)
    (insert " jslint-ignore")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alternately espresso-mode instead of js2-mode

(autoload 'espresso-mode "espresso" "espresso-mode" t)
(add-hook 'espresso-mode-hook 'run-coding-hook)
(setq espresso-indent-level 4)

(eval-after-load 'espresso
  '(progn (define-key espresso-mode-map "{" 'paredit-open-curly)
          (define-key espresso-mode-map "}" 'paredit-close-curly-and-newline)
          ;; fixes problem with pretty function font-lock
          (define-key espresso-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'espresso-mode `(("\\(function *\\)("
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1) "ƒ")
                                       nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; or
;; (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'coffee-mode)
;; (require 'flymake-coffee)
(require 'flymake-coffeelint)
(setq flymake-coffeelint-config "~/.coffeelint.json")
(setq coffee-js-mode 'js2-mode)
(setq coffee-tab-width 2)

(defun dss/coffee-electric-pair ()
  ;; this version doesn't check to see if we're inside of a string or comment
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun dss/moz-coffee ()
  (interactive)
  (save-window-excursion
    (with-current-buffer coffee-compiled-buffer-name
      ;;  (point-min) (point-max)
      (dss/moz-eval-buffer))))

(defun dss/coffee-compile-buffer ()
  "Compiles the current buffer and displays the JS in another buffer."
  (interactive)
  (save-excursion
    (save-window-excursion
      (dss/coffee-compile-region (point-min) (point-max)))))

(defun dss/coffee-compile-buffer-map-line ()
  (interactive)
  (ignore-errors
    (save-window-excursion
      (save-excursion
        (let ((source (buffer-string))
              (current-line (save-restriction
                              (widen)
                              (count-lines 1 (point)))))
          (with-temp-buffer
            (insert source)
            (goto-line current-line)
            (dss/clone-line)
            ;; (forward-line -1)
            (back-to-indentation)
            (insert "### line-marker: ")
            (end-of-line)
            (insert " ###")
            (dss/coffee-compile-buffer))))))
  (dss/coffee-jump-line))

(defun dss/coffee-jump-line ()
  (interactive)
  (let ((pnt (save-window-excursion
               (switch-to-buffer coffee-compiled-buffer-name)
               (search-forward "line-marker")
               (back-to-indentation)
               (point))))
    (dss/sync-point-all-windows (get-buffer coffee-compiled-buffer-name) pnt)))

(defun dss/coffee-send-region (start end)
  (interactive "r")
  (save-excursion
    (save-window-excursion
      (dss/coffee-compile-region start end))))

(defun dss/coffee-eval-para ()
  (interactive)
  (mark-paragraph)
  (setq mark-active nil)
  (call-interactively 'dss/flash-region)
  (call-interactively 'dss/moz-eval-region))

(defun dss/coffee-compile-region (start end)
  "Compiles a region and displays the JS in another buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      ;; (kill-buffer buffer)
      (with-current-buffer buffer
        (delete-region (point-min) (point-max)))))

  (call-process-region start end coffee-command nil
                       (get-buffer-create coffee-compiled-buffer-name)
                       nil
                       "-s" "-p" "--bare")
  (switch-to-buffer (get-buffer coffee-compiled-buffer-name))
  (ignore-errors (funcall coffee-js-mode))
  (goto-char (point-min)))

(defun dss/coffee-mode-hook ()
  (interactive)
  ;; (flymake-coffee-load)
  (flymake-mode)
  (linum-mode 1)
  (mapc (lambda (char)
          (progn
            (define-key coffee-mode-map char 'dss/coffee-electric-pair)))
        '("\"" "'" "(" "[" "{"))
  (dss/install-whitespace-cleanup-hook)
  (define-key coffee-mode-map (kbd "M-<right>") 'dss/coffee-send-region)
  (dss/add-after-save-hook 'dss/coffee-compile-buffer))

(add-hook 'coffee-mode-hook 'dss/coffee-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'dss-javascript)
