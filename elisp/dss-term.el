(require 'multi-term)
(require 'comint)
(require 'dss-vc)

;;; study this http://snarfed.org/why_i_run_shells_inside_emacs
;;; and this http://snarfed.org/emacsclient_in_tramp_remote_shells

(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t)

(setq shell-command-switch "-lc")
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term")
(setq multi-term-program "/bin/bash")
(setq multi-term-program-switches "-l")
(setq multi-term-switch-after-close nil)

;;; screen -ls | perl -ne '/\.emacs\.([^\s]+)/ && print $1'
(defun dss/cd-multi-term (dir &optional command switch buffer-name)
  (let* (term-buffer
         (default-directory dir)
         (index 1)
         (term-name (format "term-local<%s>" index))
         (term-command "eterm-local"))
    (if buffer-name
        (setq term-name buffer-name)
      (progn
        (while (buffer-live-p (get-buffer (format "*%s*" term-name)))
          (setq index (1+ index))
          (setq term-name (format "term-local<%s>" index)))))
    (setq term-buffer
          (make-term term-name term-command nil (concat "emacs." term-name)))
    (set-buffer term-buffer)
    (multi-term-internal)
    (if buffer-name
        (rename-buffer buffer-name))
    (save-window-excursion
      (switch-to-buffer term-buffer)
      (sleep-for 0.6)
      (when (< (1+ (count-lines 1 (point))) 15)
        (term-send-raw-string "source ~/_bin/setup-tramp\n")
        (when command
          (term-send-raw-string command))))
    (unless (and (not (eq switch nil))
                 (< switch 0))
      (switch-to-buffer term-buffer))
    term-buffer))

(defun dss/remote-term (host &optional command term-command before-tramp-callback)
  (interactive "sWhere: ")
  (let* (term-buffer
         (index 1)
         (term-command (or term-command "eterm-ssh"))
         term-name)
    (while (buffer-live-p
            (get-buffer (format "*%s<%s>*" host index)))
      (setq index (1+ index)))

    (setq term-name (format "%s<%s>" host index))
    (setq term-buffer
          (make-term term-name term-command nil term-name host))
    (set-buffer term-buffer)

    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer)
    (sleep-for 1)
    (if before-tramp-callback
        (funcall before-tramp-callback))
    (dss/term-setup-tramp)
    (if command
        (term-send-raw-string command))))


;; (defun dss/reconnect-term ()
;;   (interactive)
;;   ;;(term-exec buffer name program startfile switches)
;;   (let* ((buffer (current-buffer))
;;          (bufname (buffer-name buffer))
;;          (term-name (substring bufname 1 (- (length bufname) 1))))
;;     (term-exec buffer term-name "screen" nil (list "-rx" "-e^Uu" "-S" term-name))))

(defun dss/multi-term ()
  (interactive)
  (if (string-match-p tramp-file-name-regexp default-directory)
      (let ((host (second (split-string default-directory ":")))
            (dir (third (split-string default-directory ":"))))
        (dss/remote-term
         host
         (concat "cd " dir "; clear\n")))
    (multi-term)))

(defun dss/term-toggle-mode ()
  "Toggle between term-char-mode and term-line-mode."
  (interactive)
  (if (term-in-line-mode)
      (dss/term-char-mode)
    (dss/term-line-mode)))

(defun dss/term-char-mode ()
  (interactive)
  (term-char-mode)
  (linum-mode -1)
  (comint-goto-process-mark))

(defun dss/term-line-mode ()
  (interactive)
  (term-line-mode)
  (linum-mode 1))

(defun dss/term-dabbrev ()
  (interactive)
  (let ((beg (point)))
    (dabbrev-expand nil)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun dss/term-insert-path ()
  (interactive)
  (let ((beg (point)))
    (call-interactively 'dss/insert-path)
    (kill-region beg (point)))
  (term-send-raw-string (substring-no-properties (current-kill 0))))

(defun dss/term-local-path (path)
  (interactive)
  (if (file-remote-p path)
      (tramp-file-name-localname
       (tramp-dissect-file-name path))
    path))

(defun dss/term-toggle-filename-rel-abs ()
  (interactive)
  (let* ((fn (with-no-warnings
               (if (eq ido-use-filename-at-point 'guess)
                   (ffap-guesser)
                 (ffap-string-at-point))))
         (fn (replace-regexp-in-string
              "^~/" (concat (getenv "HOME") "/")
              fn))
         (replacement
          (if (file-name-absolute-p fn)
              (replace-regexp-in-string
               (dss/term-local-path default-directory) "" fn nil t)
            (expand-file-name fn)))
         (replacement (dss/term-local-path replacement))
         (replacement
          (replace-regexp-in-string (getenv "HOME") "~" replacement nil t)))
    (dss/term-backward-kill-word)       ; must be at end
    (term-send-raw-string replacement)))

(defun dss/term-insert-path-int (ido-fn &optional match-at-point no-insert)
  (let (fn
        initial
        ido-current-directory
        (ido-enable-last-directory-history nil))
    (when (and match-at-point
               (setq fn (with-no-warnings
                          (if (eq ido-use-filename-at-point 'guess)
                              (ffap-guesser)
                            (ffap-string-at-point))))
               (not (string-match "^http:/" fn))
               (let ((absolute-fn (expand-file-name fn)))
                 (setq d (if (file-directory-p absolute-fn)
                             (file-name-as-directory absolute-fn)
                           (file-name-directory absolute-fn))))
               (file-directory-p d))
      (setq ido-current-directory d)
      (setq initial (file-name-nondirectory fn)))
    (let* ((result (funcall ido-fn
                            "path: "
                            ido-current-directory nil nil
                            initial))
           (result (dss/term-local-path result))
           (short-result (replace-regexp-in-string
                          (getenv "HOME") "~" result nil t)))
      (when (not no-insert)
        (when (and fn initial)
          (dss/term-backward-kill-word))
        (term-send-raw-string short-result))
      result)))

(defun dss/term-insert-file-path ()
  (interactive)
  (dss/term-insert-path-int 'ido-read-file-name t))

(defun dss/term-insert-dir-path ()
  (interactive)
  (dss/term-insert-path-int 'ido-read-directory-name t))

(defun dss/term-insert-vc-target ()
  (interactive)
  (term-send-raw-string (dss/vc-choose-target)))

(defun dss/term-cd-dir-path ()
  (interactive)
  (let ((dir (dss/term-insert-path-int 'ido-read-directory-name nil t)))
    (dss/term-cd dir)))

(defun dss/term-get-dedicated-buffer ()
  (interactive)
  (or (let ((win (dss/term-get-current-window)))
        (when win
          (window-buffer win)))
      (save-excursion
        (if (get-buffer "*MULTI-TERM-DEDICATED*")
            (get-buffer "*MULTI-TERM-DEDICATED*")))))

(defun dss/term-get-current-window ()
  (if (multi-term-dedicated-exist-p)
      multi-term-dedicated-window
    (car
     (delq nil
           (mapcar (lambda (w)
                     (if (eq 'term-mode
                             (buffer-local-value 'major-mode
                                                 (window-buffer w)))
                         w))
                   (window-list))))))

(defun dss/term-select-window ()
  (interactive)
  (select-window
   (dss/term-get-current-window)))

(defun dss/term-cd (&optional dir term-buffer)
  (interactive)
  (let* ((dir (or dir default-directory))
         (dir (replace-regexp-in-string
               "^~/" (concat (getenv "HOME") "/")
               dir))
         (term-buffer (or term-buffer
                          (if (eq major-mode 'term-mode)
                              (current-buffer)
                            (window-buffer (dss/term-get-current-window))))))
    (if term-buffer
        (with-current-buffer term-buffer
          (term-send-raw-string (format "cd '%s'\n" dir)))
      (term-send-raw-string (format "cd '%s'\n" dir)))))

(defun dss/term-eval-string (command-string &optional term-buffer)
  (interactive)
  (let* ((term-buffer (or term-buffer
                          (if (eq major-mode 'term-mode)
                              (current-buffer)
                            (window-buffer (dss/term-get-current-window))))))
    (if term-buffer
        (with-current-buffer term-buffer
          (term-send-raw-string
           (format "%s\n" command-string
                   ;; (dss/chomp command-string)
                   ))))))

(defun dss/term-eval-region (&optional beg end)
  (interactive "r")
  (dss/term-eval-string (buffer-substring beg end)))

(defun dss/term-eval-buffer ()
  (interactive)
  (dss/term-eval-region (point-min) (point-max)))

(defun dss/term-source-buffer ()
  (interactive)
  (dss/term-eval-string
   (format "source '%s'" (expand-file-name (buffer-file-name)))))

(defun dss/term-eval-region-or-para ()
  (interactive)
  (save-excursion
    (when (not mark-active)
      (mark-paragraph)
      (setq mark-active nil)
      (call-interactively 'dss/flash-region))
    (call-interactively 'dss/term-eval-region)))

(defun dss/term-backward-kill-word ()
  (interactive)
  (if (term-in-line-mode)
      (backward-kill-word 1)
    (term-send-backward-kill-word)))

(defun dss/term-yank ()
  (interactive)
  (if (term-in-line-mode)
      (yank)
    (term-paste)))

(defun dss/term-reverse-search ()
  (interactive)
  (if (term-in-line-mode)
      (isearch-backward)
    (term-send-reverse-search-history)))

;(defun term-forward-search ()
;  (interactive)
;  (if (term-in-line-mode)
;      (isearch-forward)
;    (term-send-forward-search-history)))


;;; derived from http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(defun dss/term-setup-tramp ()
  "Setup ansi-term/tramp remote directory tracking
   NOTE:  this appears to have some sort of timing bug in it and doesn't always work"
  (interactive)
  (term-send-raw-string
   (concat "
function eterm_set_variables {
    local emacs_host=\"" (car (split-string (system-name) "\\.")) "\"
    local tramp_hostname=${TRAMP_HOSTNAME-$(hostname)}
    if [[ $TERM == \"eterm-color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033AnSiTu\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033AnSiTh\" $tramp_hostname
        fi
        echo -e \"\\033AnSiTc\" $(pwd)
    elif [[ $TERM == \"screen\" || $TERM  == \"screen-256color\" ]]; then
        if [[ $tramp_hostname != \"$emacs_host\" ]]; then
            echo -e \"\\033P\\033AnSiTu\\033\\\\\" ${TRAMP_USERNAME-$(whoami)}
            echo -e \"\\033P\\033AnSiTh\\033\\\\\" $tramp_hostname
        fi
        echo -e \"\\033P\\033AnSiTc\\033\\\\\" $(pwd)
    fi
}
function eterm_tramp_init {
    for temp in cd pushd popd; do
        alias $temp=\"eterm_set_cwd $temp\"
    done

    # set hostname, user, and cwd now
    eterm_set_variables
}
function eterm_set_cwd {
    $@
    eterm_set_variables
}
eterm_tramp_init
export -f eterm_tramp_init
export -f eterm_set_variables
export -f eterm_set_cwd
clear
echo \"tramp initialized\"
")))

(defun dss/term-mode-hook ()
  (interactive)
  (define-key term-mode-map (kbd "M-/")
    'dss/term-dabbrev)
  (define-key term-mode-map (kbd "C-c C-j")
    'dss/term-toggle-mode)
  (define-key term-mode-map (kbd "M-DEL")
    'dss/term-backward-kill-word)
  (define-key term-mode-map (kbd "M-RET")
    'find-file-at-point)
  (define-key term-mode-map (kbd "M-g")
    'dss/term-toggle-mode)
  (linum-mode -1))

(add-hook 'term-mode-hook 'dss/term-mode-hook)

(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)
        ("C-x C-x" . term-send-raw)
        ("C-x C-e" . (lambda ()
                       (interactive)
                       (term-send-raw-string "\C-x\C-e")))
                                        ;("C-p" . term-send-raw);previous-line)
                                        ;("C-n" . term-send-raw);next-line)
        ("C-s" . isearch-forward)
        ("C-r" . dss/term-reverse-search)
        ("C-m" . term-send-raw)

        ("M-/" . dss/term-dabbrev)
        ("M-RET" . find-file-at-point)
        ("M-DEL" . dss/term-backward-kill-word)
        ("M-`" . dss/term-insert-path)
        ("M-k" . term-send-raw-meta)
        ("M-y" . term-send-raw-meta)
        ("M-u" . term-send-raw-meta)
        ("C-M-k" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-k")))
        ("C-M-l" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-l")))
        ("C-M-d" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-d")))
        ("C-M-t" . (lambda ()
                     (interactive)
                     (term-send-raw-string "\e\C-t")))
        ("M-h" . term-send-raw-meta)
        ("M-s" . term-send-raw-meta)
        ("M-t" . term-send-raw-meta)

        ("M-c" . term-send-raw-meta)
        ("M-l" . term-send-raw-meta)
        ("M-|" . term-send-raw-meta)

        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("M-N" . term-send-backward-kill-word)
        ("M-r" . term-send-reverse-search-history)

        ("M-," . term-send-input)

        ("M-." . comint-dynamic-complete)
        ("Od" . term-send-backward-word)
        ("Oc" . term-send-forward-word)
        ("M-d" . term-send-forward-kill-word)
        ("M-g" . dss/term-toggle-mode)
        ("C-y" . dss/term-yank)

        ("M-]" . dss/term-cd-dir-path)
        ("M-;" . dss/term-insert-dir-path)
        ("M-'" . dss/term-insert-file-path)
        ("M-\"" . dss/term-toggle-filename-rel-abs)
        ))

;; also see http://dea.googlecode.com/svn/trunk/my-lisps/multi-term-settings.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-term)
