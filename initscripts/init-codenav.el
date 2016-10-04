;; -*- lexical-binding: t -*-

(require 'dss-codenav-helpers)
(require 'init-leaders)

(req-package 's :force t)


(req-package avy
  :general
  (pdc|with-leader
   "jb" '(avy-goto-char :which-key "avy goto char")
   "j'" '(avy-goto-char-2 :which-key "avy goto char-2")
   "jw" '(avy-goto-word-1 :which-key "avy goto word")))

(req-package swiper
  :commands (swiper swiper-all)
  :general
  (pdc|with-leader
   "s s" 'swiper
   "s S" 'swiper-all)
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
  :init
  (counsel-mode t))

(pdc|with-leader "s o" 'occur)

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
  (dired (expand-file-name "initscripts/" emacs-d)))

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

(defun pdc/find-zshrc ()
  (find-file (expand-file-name "~/.zshrc")))

(defun pdc/find-zshenv ()
  (find-file (expand-file-name "~/.zshenv")))

(cl-flet ((ff (file &rest props)
                  (let ((which-key (or (plist-get props :which-key) file)))
                    (cons (lambda ()
                            (interactive)
                            (find-file-existing (expand-file-name file)))
                          (plist-put props :which-key which-key)))
                  ))
  (pdc|with-leader
   "fe" '(:ignore t :which-key "dotfiles")
   "fed" '(pdc/find-initfile :which-key "init-real.el")
   "feD" (ff "~/.emacs.d/init.el" :which-key "init.el")
   "fei" '(pdc/find-initscripts :which-key "initscripts/")
   "fez" '(pdc/find-zshrc :which-key ".zshrc")
   "feZ" '(pdc/find-zshenv :which-key ".zshenv")
   "fek" (ff  "~/.karabiner.d/configuration/karabiner.json") :which-key "karabiner.json"))
(provide 'init-codenav)