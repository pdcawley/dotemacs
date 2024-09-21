;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-org.el -- Set up org stuff;;; Org-mode and friends

(use-package org
  :general
  (pdcmacs-leader-def
    :infix "o"
    "" '(nil :which-key "org")
    "c" 'org-capture
    "A" 'org-agenda))

(use-package org-contrib)
(use-package org-roam
  :custom
  ((org-roam-directory (file-truename (expand-file-name "~/Documents/RoamNotes/")))
   (org-use-speed-commands t))
  :init
  (setq org-roam-v2-ack t)
  :general
  (:prefix "M-m n"
   ""  '(nil :which-key "notes")
   "l" 'org-roam-buffer-toggle
   "f" 'org-roam-node-find
   "g" 'org-roam-graph
   "i" 'org-roam-node-insert
   "c" 'org-roam-capture

   "j" 'org-roam-dailies-capture-today))

;;; Org-modern

(use-package org-modern
  :after org
  :if (display-graphic-p)
  :straight
  (:type git :host github :repo "minad/org-modern")
  :hook
  (org-modern-mode . org-indent-mode)
  (org-mode . global-org-modern-mode)
  :init
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t

        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…"

        org-agenda-block-separator ?—
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "←⭠ now ───────────────────────────────────────────────"))

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

(use-package orgba
  :straight (orgba :type git :host github :repo "Fuco1/orgba"))

(use-package org-auto-tangle
  :hook org-mode
  :diminish " 🧶")

(provide 'pdcmacs-org)
