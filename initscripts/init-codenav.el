(require 'dss-codenav-helpers)
(req-package 's :force t)

(req-package avy
  :general
  (:prefix jump-leader-key
   "b" '(avy-goto-char :which-key "avy goto char")
   "'" '(avy-goto-char-2 :which-key "avy goto char-2")
   "w" '(avy-goto-word-1 :which-key "avy goto word")))

(req-package swiper
  :commands (swiper swiper-all)
  :general
  (:prefix search-leader-key
           "s" 'swiper
           "S" 'swiper-all)
  :init
  (setq ivy-height 20))


(req-package counsel
  :diminish " Ⓒ"
  :general
  ("C-x 4 C-f" '(pdc-find-file-other-window :which-key "find file other window"))
  ("C-x 4 f"   '(pdc-find-file-other-window :which-key "find file other window"))
  (:prefix files-leader-key
   "4" '(pdc-find-file-other-window :which-key "find in other window")
   "f"   '(counsel-find-file :which-key "find"))
  :config
  (counsel-mode t))

(defun swiper-mc ()
  (interactive)
  (unless (require 'multiple-cursors nil t)
    (error "multiple-cursors isn't installed"))
  (let ((cands (nreverse ivy--old-cands)))
    (unless (string= ivy-text "")
      (ivy-set-action
       (lambda (_)
         (let (cand)
           (while (setq cand (pop cands))
             (swiper--action cand)
             (when cands
               (mc/create-fake-cursor-at-point))))
         (mc/maybe-multiple-cursors-mode)))
      (setq ivy-exit 'done)
      (exit-minibuffer))))

(defun pdc/find-initfile ()
  "Edit init-real.el, in the current window."
  (interactive)
  (find-file-existing (expand-file-name "init-real.el" emacs-d)))

(defvar pdc|init-script-history nil
  "History of init scripts opened by `pdc/find-initscript'")

(defun pdc/find-initscripts ()
  "Open initscripts directory, in the current window"
  (interactive)
  (dired emacs-d))

(defun pdc//find-initscript-re-builder (str &optional greedy)
  "Re-build regex pattern from string with init prefixed"
  (ivy--regex (s-concat "^init-" str) greedy))


(defun pdc/find-initscript ()
  "Open init-file in the initscripts directory"
  (interactive)
  (ivy-read
   "Find initscript: init-" 'read-file-name-internal
   :matcher #'counsel--find-file-matcher
   :re-builder #'pdc//find-initscript-re-builder
   :action
    (lambda (x)
      (with-ivy-window
        (find-file (expand-file-name x initscripts-d))))
    :require-match 'confirm-after-completion
    :history 'pdc|init-script-history
    :caller 'pdc/find-initscript))



(general-define-key
 :prefix files-leader-key
  "e" '(:ignore t :describe "dotfiles")
  "ed" '(pdc/find-initfile :which-key "open dotfile")
  "ei" '(pdc/find-initscripts :which-key "open initscripts/"))

(provide 'init-codenav)
