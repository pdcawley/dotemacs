(req-package multi-term)
(require 'comint)

(setq comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t)

(setq shelter-and-switch "-lc")
;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; multi-term
(autoload 'multi-term "multi-term")
(setq multi-term-program "zsh")
(setq multi-term-program-switches "-l")
(setq multi-term-switch-after-close nil)

(defvar my-local-shells
  '("*shell0*" "*shell1" "*shell2*" "*shell3*" "*music*"))
(defvar my-remote-shells
  '("*devel01*"))
(defvar my-shells (append my-local-shells my-remote-shells))

(setq
 comint-input-ignoredups t
 comint-buffer-maximum-size 20000
 comint-input-ring-size 5000
 protect-buffer-bury-p nil)

(setenv "PAGER" "cat")

(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to cmmint-output-filter-functions to make stdout read only"
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook  to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

;(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on new lines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

;; make it harder to kill my shell buffers
(ignore-errors
 (req-package protbuf
   :config
   (add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)))


(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (cl-flet ((end-of-line () (goto-char (point-max))))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.el.gz")
