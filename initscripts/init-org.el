(req-package org-journal)

(req-package org
  :requires org-journal
  :diminish org-indent
  :general
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
                  :which-key "goto-last-refile"))
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



