(require 'init-leaders)

(req-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer)
  (defun dss/list-buffers-int ()
    (interactive)
    (display-buffer (list-buffers-noselect nil))))

(req-package ibuffer-vc
  :require ibuffer
  :config
  (setq ibuffer-formats
    '(
      (mark dss-modified vc-status-mini " "
        (name 35 35 :left :elide)
        ;; " " (mode 10 10 :left :elide)
        " " filename-and-process)
      ;; (mark modified read-only " " (name 18 18 :left :elide)
      ;;       " " (size 9 -1 :right)
      ;;       " " (mode 16 16 :left :elide) " " filename-and-process)
      (mark " " (name 16 -1) " " filename)))

  (define-ibuffer-column dss-modified (:name "M" :inline t)
    (if (buffer-modified-p)
    (propertize "-" 'face '(:foreground "yellow"))
      " ")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(req-package window-numbering
  :requires dash
  :general
  (pdc|with-leader :keymaps 'window-numbering-keymap
   "w1" 'select-window-1
   "w2" 'select-window-2
   "w3" 'select-window-3
   "w4" 'select-window-4
   "w5" 'select-window-5
   "w6" 'select-window-6
   "w7" 'select-window-7
   "w8" 'select-window-8

   "w0" 'bm-next
   "w9" 'bm-previous)
  :config
  (defun window-numbering-install-mode-line (&optional position)
    "Do nothing, the display is handled by spaceline.")
  (--each (number-sequence 0 9)
    (define-key window-numbering-keymap (kbd (format "M-%s" it)) nil))


  ;; (setq window-numbering-auto-assign-0-to-minibuffer nil)
  (window-numbering-mode 1))


(req-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "^\\*")) ; don't muck with special buffers

(when (fboundp 'winner-mode)
  (winner-mode 1))


                                        ; http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
(defun dss/toggle-current-window-dedication ()
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))


;;; http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun dss/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;;; http://www.emacswiki.org/emacs/TransposeWindows
(defun dss/transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;;; http://www.emacswiki.org/emacs/TransposeWindows
(defun dss/rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond
   ((not (> (count-windows) 1))
    (message "You can't rotate a single window!"))
   (t
    (let ((i 1)
          (num-windows (count-windows)))
      (while  (< i num-windows)
        (let* ((w1 (elt (window-list) i))
               (w2 (elt (window-list) (+ (% i num-windows) 1)))
               (b1 (window-buffer w1))
               (b2 (window-buffer w2))
               (s1 (window-start w1))
               (s2 (window-start w2)))
          (set-window-buffer w1 b2)
          (set-window-buffer w2 b1)
          (set-window-start w1 s2)
          (set-window-start w2 s1)
          (setq i (1+ i))))))))

(defun dss/sync-point-all-windows (&optional buffer pnt)
  (interactive)
  (let ((buffer (or buffer (current-buffer)))
        (pnt (or pnt (point))))
    (dolist (f (frame-list))
      (dolist (w (window-list f))
        (if (eq (window-buffer w) buffer)
            (set-window-point w pnt))))))

(defun dss/bury-buffer-other-windows (&optional buffer pnt)
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (save-window-excursion
      (dolist (f (frame-list))
        (dolist (w (window-list f))
          (if (eq (window-buffer w) buffer)
              (unless (window--delete w t t)
                (set-window-dedicated-p w nil)
                (switch-to-prev-buffer w 'kill))))))))

(defun dss/blank-other-frame-windows ()
  (interactive)
  (let ((buffer (get-buffer-create "*blank*")))
    (save-window-excursion
      (dolist (f (frame-list))
        (dolist (w (window-list f))
          (unless (window--delete w t t)
            (set-window-dedicated-p w nil)
            (set-window-buffer w buffer)))))))

(req-package workgroups
  :config
  (defun dss/wg-name (&optional frame)
    (with-selected-frame (or frame (selected-frame))
      (cdr (assoc 'name (wg-current-workgroup t))))))

(defun dss/frame-by-name (name)
  (dolist (fr (frame-list))
    (if (string= (frame-parameter fr 'name) name)
        (return fr))))

(defun dss/window-numbering-get-number (window frame)
  (gethash window
           (cdr (gethash frame window-numbering-table))))

(defun dss/window-sorted-frames ()
  (sort (frame-list) (lambda (a b)
                       (string-lessp
                        (frame-parameter a 'name)
                        (frame-parameter b 'name)))))
(defun dss/window-sorted-windows (frame)
  (sort (window-list f)
        (lambda (a b)
          (< (win-num a f) (win-num b f)))))

(defun dss/{} (&rest pairs)
  "http://mikael.jansson.be/log/hash-tables-and-a-wee-bit-of-sugar-in-common-lisp"
  (let ((h (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cdr do (setf (gethash key h) value))
    h))

(defun dss/window-list-data ()
  (let ((cur-win (selected-window)))
    (cl-flet ((win-num (w f) (dss/window-numbering-get-number w f)))
      (loop for f in (dss/window-sorted-frames)
            collect
            (list
             (frame-parameter f 'name)
             (loop for w in (dss/window-sorted-windows f)
                   collect (dss/{}
                            :frame f
                            :frame-name (frame-parameter f 'name)
                            :window w
                            :workgroup (dss/wg-name f)
                            :win-num (win-num w f)
                            :win-name (buffer-name (window-buffer w))
                            :selected (eq w (frame-selected-window f))
                            :current-window (eq w cur-win)
                            :point (window-point w))))))))

(defun dss/tmp-test ()
  (interactive)
  (destructuring-bind (&key a &key b) ({} :a 1 :b 2)
    (message "%s-%s" a b)))

(defun dss/-window-list ()
  (let (frame-start)
    (cl-flet ((red (s) (propertize s 'face '(:foreground "red")))
           (yellow (s) (propertize s 'face '(:foreground "yellow")))
           (grey (s) (propertize s 'face '(:foreground "#999999")))
           (current (s) (propertize
                         s 'face
                         '(:background "#222222" :foreground "yellow")))
           (wgroup (s) (propertize s 'face '(:foreground "#110000")))
           (n-to-s (n) (number-to-string n)))
      (loop for (fname wlist) in (dss/window-list-data)
            do
            (setq frame-start (point))
            (insert (format "%s\n" (red fname)))
            (dolist (wdat wlist)
              (let ((name (gethash :win-name wdat))
                    (w-sel (gethash :selected wdat))
                    (f (gethash :frame wdat))
                    (win-num (gethash :win-num wdat)))
                (insert (format "  %s  %s %s\n"
                                (grey (if (> win-num 1)
                                          (n-to-s win-num)
                                        " "))
                                (cond
                                 ((gethash :current-window wdat)
                                  (current name))
                                 ((string-equal "*blank*" name) (grey "."))
                                 (w-sel (yellow name))
                                 (t (grey name)))
                                (wgroup (or (gethash :workgroup wdat) ""))
                                ))
                ;; (put-text-property frame-start (point) 'dss-window-data wdat)
                )
              (put-text-property frame-start (point) 'frame-name fname))))))

(defun dss/window-list-enter ()
  (interactive)
  (dss/tmp-screen-switch
   (string-to-number (get-text-property (point) 'frame-name) 10))
  (quit-window))

(defun dss/window-list ()
  (interactive)
  (save-window-excursion
    (with-output-to-temp-buffer "*window-list*"
      (with-current-buffer "*window-list*"
        (dss/-window-list)
        (local-set-key (kbd "RET") 'dss/window-list-enter))))
  (pop-to-buffer "*window-list*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; workgroups

;; (setq wg-prefix-key (kbd "C-c w"))
;; (require 'workgroups)
;; (setq wg-morph-on nil
;;       wg-morph-hsteps 3
;;       wg-morph-terminal-hsteps 2
;;       wg-switch-on-load nil)
;; (workgroups-mode 1)
;; (wg-load (concat dss-ephemeral-dir "workgroups"))

;;; get-buffer-window-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dss/kill-clean-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (and buf (not (buffer-modified-p buf))
         (kill-buffer buf))))


(defun dss/kill-buffer ()
  (interactive)
  (kill-this-buffer))

(general-define-key
 :prefix "C-x"
  "k" 'kill-this-buffer)

(req-package windmove
  :loader :built-in
  :general
  ("s-<left>" 'windmove-left
   "s-<right>" 'windmove-right
   "s-<up>" 'windmove-up
   "s-<down>" 'windmove-down
   :prefix window-leader-key
   "j" 'windmove-left
   "l" 'windmove-right
   "k" 'windmove-up
   "i" 'windmove-down
   "m" 'flymake-goto-prev-error
   "," 'flymake-goto-next-error))

(req-package hydra
  :requires (ibuffer ivy)
  :init
  (setq hydra-head-format "%s → ")
  :config
  (pdc|with-leader
   "b" (list
        (defhydra hydra-buffer (nil nil :color red)
          "Buffers"
          ("B" ivy-switch-buffer-other-window "display" :color blue)
          ("b" ivy-switch-buffer "switch" :color blue)
          ("r" revert-buffer "revert")
          ("s" save-buffer "save")
          ("C" clone-buffer "clone" :color blue)
          ("k" kill-this-buffer "kill")
          ("K" kill-buffer "kill a buffer" :color blue)
          ("C-z" erase-buffer "erase" :color blue)
          ("e" eval-buffer "eval")
          ("l" ibuffer "list" :color blue)
          ("]" next-buffer "next")
          ("n" next-buffer "next")
          ("[" previous-buffer "prev")
          ("p" previous-buffer "prev")
          ("y" bury-buffer "bury")
          ("Y" unbury-buffer "unbury"))
             :which-key "buffers")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'init-buffer-and-window-handling)
