;;; -*- lexical-binding: t; -*-

(req-package org-journal
  :requires 'auto-insert
  :init
  (defun pdc/journal-title ()
    "The journal heading based on the file's name"
    (interactive)
    (let* ((year (string-to-number (substring (buffer-name) 0 4)))
           (month (string-to-number (substring (buffer-name) 4 6)))
           (day (string-to-number (substring (buffer-name) 6 8)))
           (datim (encode-time 0 0 0 day month year)))
      (format-time-string org-journal-date-format datim)))

  (defun pdc/journal-file-insert ()
    "Insert the journal heading based on the file's name."
    (interactive)
    (insert (journal-title))
    (insert "\n\n")


    (when (equal (file-name-base (buffer-file-name))
                 (format-time-string "%Y%m%d"))
      (when (file-exists-p "journal-dailies-end.org")
        (insert-file-contents "journal-dailies-end.org")
        (insert "\n"))

      (let ((weekday-template
             (downcase (format-time-string)
                       "journal-%a.org")))

        (when (file-exists-p weekday-template)
          (insert-file-contents weekday-template)))

      (when (file-exists-p "journal-dailies.org")
        (insert-file-contents "journal-dailies.org"))
      (previous-line 2)))

  :config
  (define-auto-insert "/[0-9]\\{8\\}$" [pdc/journal-file-insert]))

(req-package htmlize)

(req-package org
  :requires (org-journal htmlize)
  :diminish org-indent
  :init
  (require 'org)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (perl . t)))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-iswitchb-completing-read 'ivy-completing-read))

(req-package ox-gfm)

(with-eval-after-load 'init-leaders
  (pdc|with-leader
   "o"  '(nil :which-key "org")
   "oa" 'org-agenda
   "ob" 'org-iswitchb
   "oc" '(org-capture :which-key "capture")
   "oj" '(org-journal-new-entry :which-key "new journal")
   "ol" 'org-store-link
   "or" 'org-remember)
  (pdc|with-mode-leader :keymaps '(org-mode-map)
    "h" '(:ignore t :which-key "structure")
    "hn" '(org-insert-heading-respect-content :which-key "new")
    "hs" 'org-insert-subheading
    "c" '(:ignore t :which-key "clock")
    "ci" '(org-clock-in :which-key "in")
    "ci" '(org-clock-out :which-key "out")
    "cd" '(org-clock-mark-default-task :which-key "clock-mark-default-task")
    "c." '(org-time-stamp :which-key "time-stamp")
    "cc" '(org-clock-cancel :which-key "cancel")
    "c_" '(org-clock-select-task :which-key "select-task")
    "c -" '(org-clock-goto :which-key "goto")
    "c'" (list (lambda ()
                 (interactive)
                 (org-clock-goto '(4)))
               :which-key "goto-select-task")
    "cg" (list (lambda ()
                 (interactive)
                 (org-refile '(4)))
               :which-key "refile-goto")
    "cl" (list (lambda ()
                 (interactive)
                 (org-refile '(16)))
               :which-key "goto-last-refile")))

