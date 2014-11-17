(add-to-list 'load-path "/usr/share/emacs/site-lisp/color-theme")

;; note, this will look horrible in anything other than a 256 color
;; black or transparent black terminal window.

(defvar dss-modeline-background-color "#000087")

(defun dss/flash-modeline (&optional time color)
  (interactive)
  (let ((orig-modeline-fg (face-background 'modeline))
        (time (or time 2))
        (color (or color "#d70000")))
    (set-face-background 'modeline color)
    (run-with-timer time nil
                    'set-face-background
                    'modeline
                    orig-modeline-fg)))

(defun dss/color-theme-dark-tty ()
  (interactive)
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-install
   '(
     ((background-mode . dark)
      (border-color . "black")
      (cursor-color . "red")
      (foreground-color . "#d0d0d0")
      (mouse-color . "grey"))
     (default ((t (nil))))
     (default ((t (:foreground "#d0d0d0" :background nil))))
     (bold ((t (:bold t))))
     ;;(bold-italic ((t (:underline t :foreground "white"))))
     (highlight ((t (:background "blue" :foreground "yellow"))))
     (italic ((t (:underline t :background nil))))
     (underline ((t (:underline t))))
     ;;
     (minibuffer-prompt ((t (:foreground "yellow"))))
     (modeline ((t (:background "#000087" :foreground "#ffffaf"))))
     (modeline-inactive ((t (:background "#1c1c1c" :foreground "#ffffaf"))))
     (modeline-mousable-minor-mode ((t (:background "black" :foreground "white"))))
     (modeline-mousable ((t (:background "black" :foreground "white"))))

     ;;(scroll-bar ((t (:background "#121212"))))
     ;;(tool-bar ((t (:background "#121212" ))))
     ;;(fringe ((t (:background "#121212"))))
     (region ((t (:background "#000087" )))) ;;#0000d7 ;;#0000af
     (secondary-selection ((t (:background "#0000ff"))))
     ;;(show-paren-match-face ((t (:background "#000087"))));;#303030
     (show-paren-match-face ((t (:background "#875fd7" :foreground "white"))));;#303030
     (show-paren-mismatch-face ((t (:background "red" :foreground "blue"))))
     (flymake-errline ((t (:background "#870000"))))
     (flymake-warnline ((t (:background "#5f0000"))))
     (lineker-warning-face ((t (:background "#870000"))))

     (hl-line ((t (:background "#232323"))))
     ;; (hl-line ((t (:background "#1a1a1a"))))
     (col-highlight-face ((t (:background "#4a4a4a"))))
     (vline ((t (:background "#121212"))))

     (isearch ((t (:inherit region))))
     (isearch-secondary ((t (:inherit secondary-selection))))
     (lazy-highlight ((t (:inherit secondary-selection))))
     (match ((t (:background "#0000ff"))))
     (font-lock-builtin-face ((t (:foreground "yellow"))))
     (font-lock-comment-face ((t (:foreground "red"))))
     (font-lock-constant-face ((t (:foreground "magenta"))))
     (font-lock-function-name-face ((t (:bold t :foreground "yellow"))))
     (font-lock-keyword-face ((t (:bold t :foreground "cyan"))))
     (font-lock-pseudo-keyword-face ((t (:foreground "red"))))
     (font-lock-string-face ((t (:foreground "green"))))
     (font-lock-type-face ((t (:bold t :foreground "yellow"))))
     (font-lock-variable-name-face ((t (:foreground "cyan"))))
     (font-lock-warning-face ((t (:bold t :foreground "magenta"))))
     (custom-comment-face ((t (:foreground "red"))))
     (custom-comment-tag-face ((t (:foreground "red"))))
     (py-pseudo-keyword-face ((t (:foreground "magenta"))))
     (py-builtins-face ((t (:bold t :foreground "magenta"))))
     (ecb-tag-header-face ((t (:background "#0000ff"))))
     (speedbar-highlight-face ((t (:background "#0000ff"))))
     (whitespace-newline ((t (:background "#000087"))))
     (trailing-whitespace ((t (:background "#000087"))))
     (linum ((t (:foreground "#626262" :background "#1c1c1c"))))
     ;; #9f990e
     ;; (linum ((t (:foreground "#777777" :background "#000087"))))
     ;; (linum ((t (:foreground "white" :background "#005f00"))))
     (show-ws-tab ((t (:background "#4e4e4e"))))
     (which-func ((t (:inherit modeline)))) ;#878700
     (info-xref ((t (:inherit link :foreground "purple"))))
     (org-hide ((t (:foreground "#3a3a3a"))))
     (org-link ((t (:underline t :foreground "#afafff"))))
     (org-todo ((t (:foreground "red"))))
     (org-tag ((t (:background "#3a3a3a" :foreground "#999999"))))

     (org-agenda-clocking ((t (:background "#111111" :underline t))))
     (org-agenda-dimmed-todo-face ((t (:foreground "grey"))))
     (org-agenda-structure ((t (:foreground "yellow"))))
     (org-clock-overlay ((t (:background "#0000d7"))))
     (org-column ((t (:background "#303030"))))
     (org-column-title ((t (:background "#00005f" :underline t :weight bold))))
     (org-table ((t (:foreground "#005fff"))))
     (org-special-keyword ((t (:foreground "#999999"))))
     (org-ellipsis ((t (:foreground "#777777"))))
     (org-property-value ((t (:foreground "#bbbbbb"))))
     (org-meta-line ((t (:foreground "#767676"))))
     (org-upcoming-deadline ((t (:foreground "bright red"))))

     (outline-2 ((t (:foreground "#5fffff"))))
     (outline-3 ((t (:foreground "#00afaf"))))
     (outline-4 ((t (:foreground "#0087ff"))))
     (outline-5 ((t (:foreground "#0087af"))))

     (ac-completion-face ((t (:foreground "red"))))
     (popup-isearch-match ((t (:foreground "red"))))
     (ac-selection-face ((t (:foreground "#fffd7" :background "#554d4b"))))
     (ac-candidate-face ((t (:foreground "#fffd7" :background "#303030"))))

     (log-view-message ((t (:foreground "#0087af" :background "#1c1c1c"))))

     (highlight-changes ((t (:foreground nil :background "#382f2f"))))
     (highlight-changes-delete ((t (:foreground nil :background "#916868"))))

     (bookmarkp-local-file-without-region ((t (:foreground "white"))))

     (message-header-subject ((t (:foreground "green" :weight bold :background "#121212"))))
     (message-header-other ((t (:foreground "#6c6c6c" :background nil))))
     (message-header-xheader ((t (:foreground "#6c6c6c" :background nil))))
     (message-header-name ((t (:foreground "red" :background nil))))

     (comint-highlight-prompt ((t (:foreground "yellow"))))
     (slime-repl-input-face ((t (:background "#333333" :underline t))))
     (slime-repl-prompt-face ((t (:background "#00005f" :underline t))))

     (diff-header ((t (:foreground "#0087af" :background "#1c1c1c"))))
     (diff-file-header ((t (:foreground "#0087af" :background "#121212"))))
     (diff-refine-change ((t (:background "#1c1c1c"))))
     (diff-added ((t (:foreground "green"))))
     (diff-removed ((t (:foreground "red"))))
     (ediff-even-diff-A ((t (:background "#1c1c1c"))))
     (ediff-even-diff-B ((t (:background "#1c1c1c"))))
     (ediff-even-diff-C ((t (:background "#1c1c1c"))))
     (ediff-odd-diff-A ((t (:background "#1c1c1c"))))
     (ediff-odd-diff-B ((t (:background "#1c1c1c"))))
     (ediff-odd-diff-C ((t (:background "#1c1c1c"))))
     (ediff-current-diff-A ((t (:background "#00005f"))))
     (ediff-current-diff-B ((t (:background "#00005f"))))
     (ediff-current-diff-C ((t (:background "#00005f"))))
     (ediff-fine-diff-A ((t (:background "#5f005f"))))
     (ediff-fine-diff-B ((t (:background "#0000d7"))))
     (ediff-fine-diff-C ((t (:background "#0000d7"))))

     (magit-item-highlight ((t (:background "#161616"))))
     (magit-diff-add ((t (:foreground "green"))))
     (magit-diff-del ((t (:foreground "red"))))
     (magit-diff-file-header ((t (:foreground "#0087af" :background "#121212"))))
     (magit-diff-hunk-header ((t (:foreground "#0087ff" :background "#121212"))))

     (monky-diff-add ((t (:foreground "green"))))
     (monky-diff-del ((t (:foreground "red"))))
     (monky-log-head-label-bookmarks ((t (:foreground "red"
                                                      :background "grey"))))
     (monky-diff-hunk-header ((t (:foreground "#0087af" :background "#121212"))))

     (egg-text-base ((t (:foreground "#005faf"))))
     (egg-help-header-2 ((t (:foreground "grey"))))
     (egg-diff-file-header ((t (:foreground "#0087af" :background "#121212"))))
     (egg-diff-hunk-header ((t (:foreground "#0087ff" :background "#121212"))))
     (egg-diff-add ((t (:foreground "green"))))
     (egg-diff-del ((t (:foreground "red"))))

     ))
  (smex-update))


(defun dss/color-theme-slides ()
  (interactive)
  (dss/color-theme-dark-tty)
  (let ((color-theme-is-cumulative t))
                                        ;(color-theme-beige-eshell)
    (color-theme-install
     '(dss/color-theme-slides
       ((background-mode . dark)
        (foreground-color . "white"))
       (org-meta-line ((t (:foreground "#222222")))); 1c1c1c ; 080808
       (org-code ((t (:foreground "blue"))))

       (org-level-1 ((t (:foreground "yellow"))))
       (font-lock-comment-face ((t (:foreground "brightred"))))

       (visible-mark-face ((t (:background nil))))
       (header-line ((t (:background "black" :foreground "black"))))
       (fringe ((t (:background nil :foreground "black"))))
       (modeline ((t (:background "#090909" :foreground "#222222"))))
       (modeline-inactive ((t (:background "#090909" :foreground "#333333"))))

       ))))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (set-variable 'color-theme-is-global nil)
            (select-frame frame)
            (if window-system
                ();(color-theme-vim-colors)
              (dss/color-theme-dark-tty))))
(add-hook 'after-init-hook
          (lambda ()
            (unless window-system
              (dss/color-theme-dark-tty))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-colorthemes)
