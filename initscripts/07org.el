(add-to-list 'load-path (concat dotfiles-dir "org-mode/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "org-mode/contrib/lisp"))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)))


(setq org-link-abbrev-alist
      '(("cpan" . "http://search.cpan.org/dist/")
        ("cpansearch" . "http://search.cpan.org/search?mode=module&query=")
        ("jira" . "https://jira.dev.bbc.co.uk/browse/")
        ("gmap" . "http://maps.google.com/maps?q=%s")))
(setq org-startup-indented t)
(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "/codex.org"))
(setq org-agenda-include-all-todo t)
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)

(setq org-todo-keywords
  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
    (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)")
    (sequence "OPEN(O)" "|" "CLOSED(C)")))

(setq org-use-fast-todo-selection t)

(setq org-todo-state-tags-triggers
      '(("CANCELLED"
         ("CANCELLED" . t))
        ("WAITING"
         ("WAITING" . t))
        ("SOMEDAY"
         ("WAITING" . t))
        (done
         ("WAITING"))
        ("TODO"
         ("WAITING")
         ("CANCELLED"))
        ("NEXT"
         ("WAITING"))
        ("DONE"
         ("WAITING")
         ("CANCELLED"))))

(defun pdc/clock-in-to-next (kw)
  "Switch task from TODO to NEXT when clocking in.
Skips capture tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "NEXT"))))

(defun pdc/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

(add-hook 'org-clock-out-hook 'pdc/remove-empty-drawer-on-clock-out)

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
        "* TODO %?  %U\n  %a" :clock-in t :clock-resume t)
   ("n" "notes" entry (file+headline org-default-notes-file "Inbox and Notes")
    "* %?\n  \%U\n  %a\n  :CLOCK:\n  :END:" :clock-in t :clock-resume t)
   ("w" "Default template" entry (file+headline org-default-notes-file "Inbox and Notes")
    "*** TODO Review %c\n  %U\n  %i" :immediate-finish t :clock-in t :clock-resume t)
   ("b" "blog" entry (file (concat org-directory "/blog.org"))
    "* %U %?\n\n  %i\n  %a")
   ("a" "technology" entry (file (concat org-directory "/technology.org"))
    "* %U %?\n\n  %i\n  %a")))

(setq org-completion-use-ido t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5)))
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-allow-creating-parent-nodes 'confirm)


(setq org-agenda-files
      (mapcar (lambda (path)
                (if (string-match-p "^[/~]" path)
                    path
                  (concat org-directory path)))
              '("codex.org" "blog.org" "todo.org" "technology.org"
                "journal.org" "bbc.org")))

(defvar org-journal-file "~/Dropbox/org/journal.org"
  "Path to OrgMode journal file.")
(defvar org-journal-date-format "%Y-%m-%d"
  "Date format string for journal headings.")

(defun org-journal-entry ()
  "Create a new diary entry for today or append to an existing one."
  (interactive)
  (switch-to-buffer (find-file org-journal-file))
  (widen)
  (let ((today (format-time-string org-journal-date-format)))
    (beginning-of-buffer)
    (unless (org-goto-local-search-headings today nil t)
      ((lambda () 
         (org-insert-heading)
         (insert today)
         (insert "\n\n  \n"))))
    (beginning-of-buffer)
    (org-show-entry)
    (org-narrow-to-subtree)
    (end-of-buffer)
    (backward-char 2)
    (unless (= (current-column) 2)
      (insert "\n\n  "))))


;; Export using pygments
(defun org-export-blocks-format-pygments (body &rest headers)
  (let* ((args (if (cdr headers) (mapconcat 'identity (cdr headers) " ")))
         (data-file (make-temp-file "org-pygments"))
         (hash (progn
                 (set-text-properties 0 (length body) nil body)
                 (sha1 (prin1-to-string (list body args)))))
         (raw-out-file (if headers (car headers)))
         (out-file-parts (if (string-match "\\(.+\\)\\.\\([^\\.]+\\)$" raw-out-file)
                             (cons (match-string 1 raw-out-file)
                                   (match-string 2 raw-out-file))
                           (cons raw-out-file "pyg")))
         (out-file (concat (car out-file-parts) "_" hash "." (cdr out-file-parts))))
    (cond
     (htmlp
      (unless (file-exists-p out-file)
        ;; (mapc
        ;;  (lambda (file)
        ;;    (when (and (string-match (concat (regexp-quote (car out-file-parts))
        ;;                                     "_\\([[:alnum:]]+\\)\\."
        ;;                                     (regexp-quote (cdr out-file-parts)))
        ;;                             file)
        ;;               (= (length (match-string 1 out-file)) 40))
        ;;      (delete-file (expand-file-name file
        ;;                                     (file-name-directory out-file)))))
        ;;  (directory-files (or (file-name-directory out-file)
        ;;                       default-directory)))
        (with-temp-file data-file (insert body))
        (message (concat "pygmentize " args " -o " out-file " " data-file))
        (shell-command (concat "pygmentize " args " -f html -o " out-file " " data-file)))
      (let ((body (with-temp-buffer
                    (insert-file-contents out-file)
                    (buffer-string))))
        (concat
         "\n#+BEGIN_HTML\n"
         body
         "\n#+END_HTML\n")))
     (t (message "Dunno how to format for that")))))

(require 'org-exp-blocks)
(org-export-blocks-add-block '(pygmented org-export-blocks-format-pygments nil))

;; Agenda filtering helpers
(defun pdc/weekday-p ()
  (let ((wday (nth 6 (decode-time))))
    (and (< wday 6) (> wday 0))))

(defun pdc/working-p ()
  (let ((hour (nth 2 (decode-time))))
    (and (pdc/weekday-p) (or (and (>= hour 8) (<= hour 11))
                             (and (>= hour 13) (<= hour 16))))))

(defun pdc/network-p ()
  (= 0 (call-process "/bin/ping" nil nil nil
                     "-c1" "-q" "-t1" "www.bofh.org.uk")))

(defun pdc/org-auto-exclude-function (tag)
  (and (cond
        ((string= tag "@home")
         (pdc/working-p))
        ((string= tag "@office")
         (not (pdc/working-p)))
        ((or (string= tag "@errand") (string= tag "phone"))
         (let ((hour (nth 2 (decode-time))))
           (or (< hour 8) (> hour 21)))))
       (concat "-" tag)))

(setq org-agenda-auto-exclude-function 'pdc/org-auto-exclude-function)

;; Clocking rules
(org-clock-persistence-insinuate)
(setq
 org-clock-history-length 28
 org-clock-in-resume t
 org-clock-in-switch-to-state 'pdc/clock-in-to-next
 org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK")
 org-clock-into-drawer "CLOCK"
 org-clock-out-remove-zero-time-clocks t
 org-clock-out-when-done t
 org-clock-persist 'history
 org-clock-auto-clock-resolution 'when-no-clock-is-running
 org-clock-report-include-clocking-task t)

(setq pdc/keep-clock-running nil)

(defun pdc/clock-in ()
  (interactive)
  (setq pdc/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
        (pdc/clock-in-default-task))
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))

(defun pdc/clock-out ()
  (interactive)
  (setq pdc/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun pdc/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun pdc/clock-out-maybe ()
  (when (and pdc/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (pdc/clock-in-default-task)))

(require 'org-id)
(defun pdc/clock-in-task-by-id (id)
  "Clock in a task by id"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil))))

(defun pdc/clock-in-last-task ()
  "Clock in the interrupted task if there is one"
  (interactive)
  (let ((clock-in-to-task (if (org-clock-is-active)
                              (cadr org-clock-history)
                            (car org-clock-history))))
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))
                                    

(add-hook 'org-clock-out-hook 'pdc/clock-out-maybe 'append)
(setq org-time-stamp-rounding-minutes '(1 15))
(setq org-agenda-log-mode-items '(clock))
(setq org-agenda-clockreport-parameters-plist '(:link nil :maxlevel 3))



(setq org-columns-default-format "%80ITEM(Task) %10(Effort){:} %10CLOCKSUM")

(setq org-global-properties  '(("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00")))

(setq org-tag-alist
      '((:startgroup)
        ("@errand" . ?e)
        ("@office" . ?e)
        ("@home" . ?h)
        ("@flat" . ?f)
        (:endgroup)
        ("PHONE" . ?P)
        ("WAITING" . ?w)
        ("HOME" . ?H)
        ("NOTE" . ?n)
        ("CANCELLED" . ?C)))

(setq org-fast-tag-selection-single-key t)
(setq org-agenda-tags-todo-honor-ignore-options t)
(setq org-agenda-ndays 1)

(defun pdc/is-project-p ()
  "Any tasks with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    has-subtask))

(defun pdc/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t)))))
    (if (and (pdc/is-project-p) (not has-next))
        nil
      subtree-end)))

(defun pdc/skip-non-projects ()
  "Skip trees that are not projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (pdc/is-project-p)
        nil
      subtree-end)))

(defun pdc/skip-projects ()
  "Skip trees that are projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (pdc/is-project-p)
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("w" "Tasks waiting on something" tags "WAITING/!"
         ((org-use-tag-inheritance nil)
          (org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-overriding-header "Waiting Tasks")))
        ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-overriding-header "Tasks to Refile")))
        ("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")))
        ("n" "Next" tags-todo "-WAITING-CANCELLED/!NEXT"
         ((org-agenda-overriding-header "Next Tasks")))
        ("p" "Projects" tags-todo "LEVEL=3-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'pdc/skip-non-projects)
          (org-agenda-overriding-header "Projects")))
        ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=3-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'pdc/skip-projects)
          (org-agenda-overriding-header "Other Non-Project Tasks")))
        ("A" "Tasks to be Archived" tags "LEVEL=3-REFILE/DONE|CANCELLED"
         ((org-agenda-overriding-header "Tasks to Archive")))
        ("h" "Habits" tags "STYLE=\"habit\""
         ((org-agenda-todo-ignore-scheduled nil)
          (org-agenda-todo-ignore-deadlines nil)
          (org-agenda-todo-ignore-with-date nil)
          (org-agenda-overriding-header "Habits")))
        ("#" "Stuck Projects" tags-todo "LEVEL=3-REFILE|LEVEL=1+REFILE/!-DONE-CANCELLED"
         ((org-agenda-skip-function 'pdc/skip-non-stuck-projects)
          (org-agenda-overriding-header "Stuck Projects")))
        ("c" "Select default clocking task" tags "LEVEL=3-REFILE"
         ((org-agenda-skip-function
           '(org-agenda-skip-subtree-if 'notregexp "^\\*\\*\\* Organization"))
          (org-agenda-overriding-header "Set default clocking task with C-u C-u I")))))

(defun pdc/org-todo ()
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil))

(defun pdc/widen ()
  (interactive)
  (widen)
  (org-reveal))

(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

(setq org-agenda-todo-ignore-with-date nil
      org-agenda-todo-ignore-deadlines 'far
      org-agenda-todo-ignore-scheduled 'future
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-timestamp-if-done t)

(setq org-agenda-include-diary nil
      org-agenda-diary-file (concat org-directory "diary.org"))

(setq org-agenda-repeating-timestampe-show-all t
      org-agenda-show-all-dates t
      org-agenda-start-on-weekday nil
      org-agenda-tags-column -102)

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up priority-down effort-up category-keep)
        (todo priority-down)
        (tags priority-down)))

(setq org-agenda-time-grid
      '(nil "----------------"
            (800 1000 1200 1400 1600 1800 2000)))

(setq org-agenda-cmp-user-defined 'pdc/agenda-sort)

(defun pdc/agenda-sort (a b)
  "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
  (let (result num-a num-b)
    (cond
     ((pdc/agenda-sort-test 'pdc/is-not-scheduled-or-deadline a b))
     ((pdc/agenda-sort-test-num 'pdc/is-late-deadline '< a b))
     ((pdc/agenda-sort-test 'pdc/is-due-deadline a b))
     ((pdc/agenda-sort-test-num 'pdc/is-scheduled-late '> a b))
     ((pdc/agenda-sort-test 'pdc/is-scheduled-today a b))
     ((pdc/agenda-sort-test-num 'pdc-is-pending-deadline '< a b))
     (t (setq result nil)))
    result))

(defmacro pdc/agenda-sort-test (fn a b)
  "Test for agenda sort"
  `(setq result
    (cond
     ((and (,fn ,@a) (,fn ,@b)) nil)
     ((,fn ,@a) (if (,fn ,@b) nil -1)
     ((,fn ,@b) 1)
     (t nil)))))

(defmacro pdc/agenda-sort-test-num (fn cmpfn a b)
  `(setq result
    (cond
     ((,fn ,@a)
      (setq num-a (string-to-number (match-string 1 ,a)))
      (if (,fn ,@b)
          (progn
            (setq num-b (string-to-number (match-string 1 ,b)))
            (if (,compfn num-a num-b) -1 1))
        -1))
     ((,fn ,@b) 1)
     (t nil))))

(defun pdc/is-not-scheduled-or-deadline (date-str)
  (and (not (pdc/is-deadline date-str))
       (not (pdc/is-scheduled date-str))))

(defun pdc/is-due-deadline (date-str)
  (string-match "Deadline:" date-str))

(defun pdc/is-late-deadline (date-str)
  (string-match "In *\\(-.*\\)d\\.:" date-str))

(defun pdc/is-pending-deadline (date-str)
  (string-match "In \\([^-]*\\)d\\.:" date-str))

(defun pdc/is-deadline (date-str)
  (or (pdc/is-due-deadline date-str)
      (pdc/is-late-deadline date-str)
      (pdc/is-pending-deadline date-str)))

(defun pdc/is-scheduled (date-str)
  (or (pdc/is-scheduled-today date-str)
      (pdc/is-scheduled-late date-str)))

(defun pdc/is-scheduled-today (date-str)
  (string-match "Scheduled:" date-str))

(defun pdc/is-scheduled-late (date-str)
  (string-match "Sched\\.\\(.*\\)x:" date-str))

(require 'org-checklist)

(setq org-enforce-todo-dependencies t)
(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry '((heading) (plain-list-item)))
(setq org-insert-heading-respect-content t)
(setq org-reverse-note-order nil)

(setq org-show-following-heading t
      org-show-hierarchy-above t
      org-show-siblings nil)

(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-yank-adjusted-subtrees t)

(setq org-id-method 'uuidgen)

(setq org-deadline-warning-days 30)

(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-link-frame-setup
      '((gnus . org-gnus-no-new-news)
        (file . find-file-other-window)))

(setq org-log-done 'time
      org-log-into-drawer t)

(add-to-list 'org-global-properties '("STYLE_ALL" . "habit"))
(setq org-habit-graph-column 50)
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

(setq org-use-speed-commands t)
(setq org-speed-commands-user
      '(("0" . delete-window)
        ("1" . delete-other-windows)
        ("2" . split-window-vertically)
        ("3" . split-window-horizontally)
        ("h" . hide-other)
        ("k" . org-kill-note-or-show-branches)
        ("r" . org-reveal)
        ("s" . org-save-all-org-buffers)
        ("z" . org-add-note)))

(defun pdc/insert-inactive-timestamp ()
  (interactive)
  (org-insert-time-stamp nil t t nil nil nil))

(defun org-return-follows-link t)

(add-to-list 'Info-default-directory-list (concat dotfiles-dir "org-mode/doc"))
(setq org-read-date-prefer-future nil)

(setq org-list-demote-modify-bullet
      '(("+" . "-")
        ("*" . "-")
        ("1." . "-")
        ("1)" . "-")))

(setq org-cycle-include-plain-lists nil)

(setq org-emphasis-alist
      '(("*" bold "<b>" "</b>")
        ("/" italic "<i>" "</i>")
        ("_" italic "<em>" "</em>")
        ("=" org-code "<code>" "</code>" verbatim)
        ("~" org-verbatim "<code>" "</code>" verbatim)))


(defun pdc/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading)
    (org-shifttab)
    (org-reveal)
    (org-cycle)))

(defun pdc/org-info ()
  (interactive)
  (info (concat dotfiles-dir "org-mode/doc/org")))

(defun pdc/go-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (delete-other-windows))

(defun pdc/untabify ()
  (interactive)
  (untabify (point-min) (point-max)))

; Grab C-c o as a global org mode prefix for this stuff
(global-set-key (kbd "C-c o") nil)
(global-set-key (kbd "C-c o j") 'org-journal-entry)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "<f5>") 'pdc/org-todo)
(global-set-key (kbd "<S-f5>") 'pdc/widen)
(global-set-key (kbd "<f9> t") 'pdc/insert-inactive-timestamp)

(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f7>") 'set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;(global-set-key (kbd "<f9> b") 'bbdb)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> g") 'gnus)
(global-set-key (kbd "<f9> h") 'pdc/hide-other)
(global-set-key (kbd "<f9> i") 'pdc/org-info)
(global-set-key (kbd "<f9> I") 'pdc/clock-in)
(global-set-key (kbd "<f9> O") 'pdc/clock-out)
(global-set-key (kbd "<f9> s") 'pdc/go-to-scratch)
(global-set-key (kbd "<f9> t") 'pdc/insert-inactive-timestamp)
(global-set-key (kbd "<f9> u") 'pdc/untabify)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> SPC") 'pdc/clock-in-last-task)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "M-<f11>") 'org-resolve-clocks)
(global-set-key (kbd "C-M-r") 'org-capture)
(global-set-key (kbd "M-<f9>")
                (lambda ()
                  (interactive)
                  (unless (buffer-modified-p)
                    (kill-buffer (current-buffer)))
                  (delete-frame)))

(require 'appt)
(defun pdc/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

(add-hook 'org-finalize-agenda-hook 'pdc/org-agenda-to-appt)
(pdc/org-agenda-to-appt)
(appt-activate t)
(run-at-time "24:01" nil 'pdc/org-agenda-to-appt)

