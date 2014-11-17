;;; k2-mode.el -- A minor mode with Some keybindings and misc functions for numeric keypads

;;; I wrote k2-mode in 2001 when I was a complete newb and it's full
;;; of stuff I don't use these days.
;;;
;;; PURPOSE: to avoid hideous finger jumps and the nasty [CTRL] key,
;;; and make my most used commands readily accessible
;;;
;;; NOTES:
;;; See http://xahlee.org/emacs/keyboard_shortcuts.html
;;;
;;; (global-set-key [f5] 'goto-buffers-list)
;;; or use (define-key k2-mode-map [f7] ctl-x-map)
;;; or something like this vector list (global-set-key [f6] "\C-x\C-c")
;;; or maybe something like this (keyboard-translate [f6] ?\C-x)
;;;
;;; don't use Scroll-Lock (key-20) !  it interferes with the function keys


(defun k2-backward-comment ()
  (interactive)
  (search-backward comment-start))

(defun k2-forward-comment ()
  (interactive)
  (search-forward comment-start))

(defun k2-mark-comment ()
  (interactive)
  (beginning-of-line)
  (search-forward comment-start)
  (backward-char (length comment-start))
  (set-mark-command nil)
  (end-of-line))
(defun k2-mark-whole-sexp ()
  (interactive)
  (backward-sexp 1)
  (mark-sexp))
(defun k2-mark-whole-sentence ()
  (interactive)
  (backward-sentence 1)
  (set-mark-command nil)
  (forward-sentence))
(defun k2-mark-whole-line ()
  (interactive)
  (beginning-of-line)
  (set-mark-command nil)
  (end-of-line))

(defun k2-kill-whole-sexp ()
  (interactive)
  (mark-sexp 1)
  (kill-sexp 1))
(defun k2-kill-whole-word ()
  (interactive)
  (mark-word 1)
  (kill-word 1))
(defun k2-kill-whole-paragraph ()
  (interactive)
  (mark-paragraph)
  (kill-region (region-beginning) (region-end)))
(defun k2-kill-whole-line ()
  (interactive)
  (k2-mark-whole-line)
  (kill-region (region-beginning) (region-end)))
(defun k2-kill-whole-defun ()
  (interactive)
  (mark-defun)
  (kill-region (region-beginning) (region-end)))

(defun k2-copy-comment ()
  (interactive)
  (save-excursion
    (k2-mark-comment)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-whole-sexp ()
  (interactive)
  (save-excursion
    (mark-sexp 1)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-whole-word ()
  (interactive)
  (save-excursion
    (mark-word)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-whole-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-whole-line ()
  (interactive)
  (save-excursion
    (k2-mark-whole-line)
    (kill-ring-save (region-beginning) (region-end))))
(defun k2-copy-whole-defun ()
  (interactive)
  (save-excursion
    (mark-defun)
    (kill-ring-save (region-beginning) (region-end))))

(defun k2-toggle-mark ()
  (interactive)
  (cond
   ((not (equal mark-active nil))
    (setq mark-active nil))
   ((equal mark-active nil)
    (setq mark-active t))))

(defun k2-mark-rest-of-buffer()
  (interactive)
  (set-mark-command nil)
  (goto-char (point-max)))


(defun k2-horiz-rule(c)
  "make a horizontal rule on the next line that spans all the text on the current line"
  (interactive "s")
  (end-of-line)
  (let ((l (current-column)))
    (insert "\n")
    (insert-char (string-to-char c) l)
    (insert "\n")))

(defvar k2-mode nil
  "On/Off switch variable for k2-mode")

(defvar k2-mode-map nil
  "Local keymap for k2-mode buffers")

(if k2-mode-map nil
    (setq k2-mode-map (make-sparse-keymap))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Extra Prefix Keys - for standard maps and
    ;; for my custom ones
    ;;

    ;; set prefix keys for C-x-C-[char] shortcuts  --- see below
    (defvar prefix-kp-multiply-map (make-keymap))
    (define-key k2-mode-map [kp-multiply] prefix-kp-multiply-map)
    (define-key k2-mode-map [f9] prefix-kp-multiply-map)

    ;; set prefix keys for M-C-[char] shortcuts   --- see below
    (defvar prefix-kp-subtract-map (make-keymap))
    (define-key k2-mode-map [kp-subtract] prefix-kp-subtract-map)
    (define-key k2-mode-map [f11] prefix-kp-subtract-map)

    (defvar prefix-kp-add-map (make-keymap))
    (define-key k2-mode-map [kp-add] prefix-kp-add-map)

    (defvar prefix-kp-divide-map (make-keymap))
    (define-key k2-mode-map [kp-divide] prefix-kp-divide-map)


    (defvar prefix-s-kp-divide-map (make-keymap))
    (define-key k2-mode-map [s-kp-divide] prefix-s-kp-divide-map)


    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; Basic Redefinitions
    ;;

    ;; random functions
    (define-key k2-mode-map [s-end]     'k2-horiz-rule) ;


    ;; Basic File-Buffer-Program Operations (Open, Save, Close, Write, Open Desktop, Exit)


    (define-key k2-mode-map [C-kp-down] 'find-file) ; C-x C-f
    (define-key k2-mode-map [s-M-kp-down] 'find-file-read-only) ; C-x C-r
    (define-key k2-mode-map [s-C-M-kp-down] 'find-alternate-file) ; note- same as C-M-S-kp-begin
    (define-key k2-mode-map [s-kp-down] 'save-buffer) ; C-x C-s
    (define-key k2-mode-map [s-return] 'save-buffer) ; C-x C-s
    (define-key k2-mode-map [C-M-kp-2] 'kill-buffer) ; C-x k - Same as C-M-S-kp-down

    (define-key k2-mode-map [M-kp-down] 'write-file)
    (define-key k2-mode-map [M-kp-down] 'write-region)
    (define-key k2-mode-map [C-M-kp-down] '(lambda (file)
                                           (interactive "FWrite to file: ")
                                           (write-region (region-beginning)
                                                         (region-end) file t))) ; appends region to file
    ;;  Insert/ Delete / Undo  Commands
    (define-key k2-mode-map [C-insert] 'yank-pop) ; M-y
    (define-key k2-mode-map [C-M-insert] 'yank-rectangle)
    (define-key k2-mode-map [M-insert] 'insert-register)
    (define-key k2-mode-map [C-M-S-insert] 'insert-file)

    ;; Basic Cursor/Point Movement Commands

                                        ; M-previous 'Pg Up' and M-next 'Pg Dn' to
                                        ; scroll to other window

    (define-key k2-mode-map [home] 'beginning-of-line)
    (define-key k2-mode-map [end] 'end-of-line)

    ;; Block Cursor/Point Movement Commands
    (define-key k2-mode-map [\e up]        'backward-paragraph) ; up
    ;; Mark Commands

    (define-key k2-mode-map [C-kp-add] 'k2-toggle-mark) ; [C-+] toggle the mark active/not

    (define-key prefix-kp-add-map [kp-home] 'mark-word) ; [7] word
    (define-key prefix-kp-add-map [kp-prior] 'mark-paragraph) ; [9] paragraph

    (define-key prefix-kp-add-map [kp-left] 'k2-mark-whole-line) ; [4] line
    (define-key prefix-kp-add-map [kp-begin] 'mark-sexp) ; [5] sexp
    (define-key prefix-kp-add-map [kp-right] 'mark-defun) ; [6] defun

    (define-key prefix-kp-add-map [kp-end] 'k2-mark-comment) ; [1] comment
    (define-key prefix-kp-add-map [kp-down] 'mark-whole-buffer) ; [2] buffer - not applicable

    (define-key prefix-kp-add-map [delete] 'point-to-register) ; [.] register - point to register
    (define-key prefix-kp-add-map "\C-d" 'point-to-register) ; [.] register - point to register

    ;; Kill Commands
    (define-key prefix-kp-subtract-map [kp-home] 'k2-kill-whole-word) ; [7] word
    (define-key prefix-kp-subtract-map [kp-prior] 'k2-kill-whole-paragraph) ; [9] paragraph

    (define-key prefix-kp-subtract-map [kp-left] 'k2-kill-whole-line) ; [4] line
    (define-key prefix-kp-subtract-map [kp-begin] 'k2-kill-whole-sexp) ; [5] sexp
    (define-key prefix-kp-subtract-map [kp-right] 'k2-kill-whole-defun) ; [6] defun

    (define-key prefix-kp-subtract-map [kp-end] 'kill-comment) ; [1] comment
    (define-key prefix-kp-subtract-map [kp-down] 'kill-buffer) ; [2] buffer - not applicable
    (define-key prefix-kp-subtract-map [kp-next] 'kill-rectangle) ; [3] rectangle -
                                                                  ; retrieve it with
                                                                  ; [kp-add kp-next]
                                        ; add x-clipboard functionality


    ;; Copy Commands
                                        ; Add x-clipboard stuff
                                        ;

    (define-key prefix-kp-multiply-map [kp-multiply] 'kill-ring-save) ;

    (define-key prefix-kp-multiply-map [kp-home] 'k2-copy-whole-word) ; [7] word
    (define-key prefix-kp-multiply-map [kp-prior] 'k2-copy-whole-paragraph) ; [9] paragraph

    (define-key prefix-kp-multiply-map [kp-left] 'k2-copy-whole-line) ; [4] line
    (define-key prefix-kp-multiply-map [kp-begin] 'k2-copy-whole-sexp) ; [5] sexp
    (define-key prefix-kp-multiply-map [kp-right] 'k2-copy-whole-defun) ; [6] defun

    (define-key prefix-kp-multiply-map [kp-end] 'k2-copy-comment) ; [1] comment
    (define-key prefix-kp-multiply-map [kp-down] 'k2-copy-buffer) ; [2] buffer
    (define-key prefix-kp-multiply-map [kp-next] '(lambda ()
                                                    (interactive)
                                                    (save-excursion
                                                      (kill-rectangle
                                                       (region-beginning) (region-end))
                                                      (exchange-point-and-mark)
                                                      (yank-rectangle))))


                                        ; Register Copy Commands
    (define-key prefix-kp-multiply-map [delete] 'copy-to-register) ; [.] copies the
                                                                   ; region to
                                                                   ; register
    (define-key prefix-kp-multiply-map [M-delete] 'copy-rectangle-to-register) ; [3]
                                                                               ; rectangle
                                                                               ; to
                                                                               ; register

    ;; Comment Commands

                                        ; M-kp-divide to comment and C-M-kp-divide to
                                        ; uncommment

    (define-key k2-mode-map [M-kp-divide kp-divide] 'comment-region) ;

    ;; Un-Comment Commands
    (define-key k2-mode-map [C-M-kp-divide kp-divide] '(lambda ()
                                                             (interactive)
                                                             (comment-region
                                                              (region-beginning) (region-end) -1)))


    ;; Block Comment Commands

                                        ; C-M-S-kp-divide - create (auto-name
                                        ; matching for either side of block) -
                                        ; movement - outline show/hide - field move -
                                        ; region-select

    ;; Transposition Commands
    (define-key prefix-kp-divide-map [kp-divide] 'exchange-point-and-mark) ;

    (define-key prefix-kp-divide-map [kp-home] 'transpose-words) ; [7] word
    (define-key prefix-kp-divide-map [kp-up] 'transpose-sentences) ; [8] sentence
    (define-key prefix-kp-divide-map [kp-prior] 'transpose-paragraphs) ; [9] paragraph

    (define-key prefix-kp-divide-map [kp-left] 'transpose-lines) ; [4] line
    (define-key prefix-kp-divide-map [kp-begin] 'transpose-sexps) ; [5] sexp
                                        ; [6] defun - transposition not possible yet


    ;; Rectangle Commands
                                        ;'copy-rectangle-to-register
                                        ;'string-rectangle
                                        ;'open-rectangle
                                        ;'clear-rectangle
                                        ;'close-rectangle
                                        ;'delete-rectangle
                                        ;'yank-rectangle
                                        ;'delete-whitespace-rectangle

    ;; Eval Commands  M-*


                                        ; add hooks for relevant modes

                                        ; plus prefix for count, case, spell, print,
                                        ; shell-command, reverse, TAC, filter-region
                                        ; through shell command, etc.

                                        ; reverse-region, ispell-region,
                                        ; print-region, upcase-region,
                                        ; shell-command-on-region,
                                        ; count-lines-region, capitalize-region,
                                        ; eval-region, spell-region



                                        ; eval-last-sexp C-x C-e Command: Evaluate
                                        ;   sexp before point; print value in
                                        ;   minibuffer.

                                        ; eval-print-last-sexp M-x
                                        ;   eval-print-last-sexp RET Command:
                                        ;   Evaluate sexp before point; print value
                                        ;   into current buffer.

                                        ; indent-sexp M-x indent-sexp RET Command:
                                        ; Indent each line of the list starting just
                                        ; after point.

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; keymap definition for C-x-C-[char] shortcuts - the prefix keys are f9 and kp-multiply *

    (define-key prefix-kp-multiply-map "2" 'pop-global-mark) ;shortcut to \C-x\C-@
    (define-key prefix-kp-multiply-map "b" 'list-buffers)
    (define-key prefix-kp-multiply-map "c" 'save-buffers-kill-emacs)
    (define-key prefix-kp-multiply-map "d" 'list-directory)
    (define-key prefix-kp-multiply-map "e" 'eval-las-sexp)
    (define-key prefix-kp-multiply-map "f" 'find-file)
    (define-key prefix-kp-multiply-map "k" 'edit-kbd-macro)
    (define-key prefix-kp-multiply-map "l" 'downcase-region)
    (define-key prefix-kp-multiply-map "n" 'set-goal-column)
    (define-key prefix-kp-multiply-map "o" 'delete-blank-lines)
    (define-key prefix-kp-multiply-map "p" 'mark-page)
    (define-key prefix-kp-multiply-map "q" 'vc-toggle-read-only)
    (define-key prefix-kp-multiply-map "r" 'find-file-read-only)
    (define-key prefix-kp-multiply-map "s" 'save-buffer)
    (define-key prefix-kp-multiply-map "t" 'transpose-lines)
    (define-key prefix-kp-multiply-map "v" 'find-alternate-file)
    (define-key prefix-kp-multiply-map "w" 'write-file)
    (define-key prefix-kp-multiply-map "x" 'exchange-point-and-mark)
    (define-key prefix-kp-multiply-map "z" 'iconify-or-deiconify-frame)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;
    ;; keymap definition for M-C-[char] shortcuts - the prefix keys are f11 and kp-subtract [-]
    ;;
    ;; -also used as 'mark then kill' prefix for the keypad normal keys (not the numbers)

    (define-key prefix-kp-subtract-map "@" 'mark-sexp)
    (define-key prefix-kp-subtract-map "a" 'beginning-of-defun)
    (define-key prefix-kp-subtract-map "b" 'backward-sexp)
    (define-key prefix-kp-subtract-map "c" 'exit-recursive-edit)
    (define-key prefix-kp-subtract-map "d" 'down-list)
    (define-key prefix-kp-subtract-map "e" 'end-of-defun)
    (define-key prefix-kp-subtract-map "f" 'forward-sexp)
    (define-key prefix-kp-subtract-map "h" 'mark-defun)
    (define-key prefix-kp-subtract-map "j" 'indent-new-comment-line)
    (define-key prefix-kp-subtract-map "k" 'kill-sexp)
    (define-key prefix-kp-subtract-map "l" 'reposition-window)
    (define-key prefix-kp-subtract-map "n" 'forward-list)
    (define-key prefix-kp-subtract-map "o" 'split-line)
    (define-key prefix-kp-subtract-map "p" 'backward-list)
    (define-key prefix-kp-subtract-map "r" 'isearch-backward-regexp)
    (define-key prefix-kp-subtract-map "s" 'isearch-forward-regexp)
    (define-key prefix-kp-subtract-map "t" 'transpose-sexps)
    (define-key prefix-kp-subtract-map "u" 'backward-up-list)
    (define-key prefix-kp-subtract-map "v" 'scroll-other-window)
    (define-key prefix-kp-subtract-map "w" 'append-next-kill)
    (define-key prefix-kp-subtract-map "\\" 'indent-region))


(defun k2-mode (&optional arg)
  "Shortcut keymap minor mode"
  (interactive "P")
  (setq k2-mode (not (or (and (null arg) k2-mode)
                         (<= (prefix-numeric-value arg) 0))))
  (or (assoc 'k2-mode minor-mode-alist)
      (setq minor-mode-alist
            (cons '(k2-mode " K2") minor-mode-alist)))
  (or (assoc 'k2-mode minor-mode-map-alist)
      (setq minor-mode-map-alist (cons (cons k2-mode k2-mode-map)
                                       minor-mode-map-alist))))
(provide 'k2-mode)
