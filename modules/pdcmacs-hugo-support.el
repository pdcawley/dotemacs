;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-hugo-support.el -- Sets up our hugo support using ox-hugo

(eval-when-compile
  (require 'cl-macs))

(defun week< (string time)
  (string< string (format-time-string "%Y-%m-%d" time)))

(defun weekday< (string time)
  (< (1+ (-elem-index string '("Monday" "Tuesday" "Wednesday"
                               "Thursday" "Friday" "Saturday"
                               "Sunday")))
     (string-to-number (format-time-string "%u" time))))

(defun pdc:filename-for-week (time)
  (let ((eow-d (-> time time-to-days
                   calendar-gregorian-from-absolute
                   pdc:end-of-week-date)))
    (format "week-ending-%d%d%d"
            (calendar-extract-year eow-d)
            (calendar-extract-month eow-d)
            (calendar-extract-day eow-d))))

(defvar +org-weeknotes-formats
  '(("%Y" :comparator #'string<)
    ("Week ending %Y-%m-%d"
     :comparator #'week<
     :matcher (rx "Week ending "
                  (group-n 1
                    (= 4 (any digit))
                    (= 2 (group
                          "-"
                          (= 2 (any digit))))))
     :properties (:export_file_name #'pdc:file-name-for-week))
    ("%A"
     :comparator #'weekday<)))

;;; I really dislike the practice of `defvar'ing private variables, so
;;; we introduce a let form in which define
;;; `+org-pathbuilder-find-create-path' and `+org-pathbuilder-insert-line'
;;; to allow them to share a private `current-level' variable.
;;;
;;; TODO: make the treebuilder state something that's passed around --
;;; I want to be able to clean up after an aborted capture, for instance.
(let ((current-level))
  (defun +org-pathbuilder-find-create-path (keep-restriction pathspec)
    "Find or create a place in FILE at the PATHSPEC given."
    (when pathspec
      (save-restriction
        (cond ((eq keep-restriction 'subtree-at-point)
               (unless (org-at-heading-p) (error "Not at heading"))
               (widen)
               (org-narrow-to-subtree))
              ((not keep-restriction)
               (widen)))
        (goto-char (point-min))
        (setq current-level (org-get-valid-level (or (org-current-level) 0) 1))
        (when pathspec
          (let* ((targetspec (-list (car pathspec)))
                 (remainder (cdr pathspec))
                 (target (car targetspec))
                 (insertion (or (cadr targetspec) target)))
            (+org-pathbuilder--find-create target insertion)
            (+org-pathbuilder-find-create-path 'subtree-at-point remainder))))))

  (defun +org-pathbuilder-insert-line (insert)
    (delete-region
     (save-excursion (skip-chars-backward " \t\n") (point))
     (point))
    (when (org--blank-before-heading-p) (insert "\n"))

    (insert "\n"
            (make-string current-level ?*)
            " \n")
    (backward-char)
    (insert insert)
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (widen)))

(defun +org-pathbuilder--find-create (target insert)
  (let* ((target-regex (cond ((string-match-p "\\\\(\\?1:" target)
                              target)
                             ((string-match "\\\\(" target)
                              (replace-match "\\(?1:" nil t target))
                             (t
                              (s-wrap target "\\(?1:" "\\)"))))
         (re (format org-complex-heading-regexp-format target-regex))
         (match))
    (goto-char (point-min))
    (when (setq match (re-search-forward re nil t))
      (goto-char (match-beginning 1)))
    (cond ((not match)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (+org-pathbuilder-insert-line insert))
          (t (forward-line 0)))))

(defun pdc:weeknote-path (&optional d)
  (let* ((d (or d (-> (org-current-effective-time)
                      time-to-days
                      calendar-gregorian-from-absolute)))
         (eow-d (pdc:end-of-week-date d))
         (eow-time (org-encode-time 0 0 0
                                    (calendar-extract-day eow-d)
                                    (calendar-extract-month eow-d)
                                    (calendar-extract-year eow-d))))
    (list (format-time-string "%Y" eow-time)
          (let* ((heading-title (format-time-string "Week ending %Y-%m-%d" eow-time))
                 (full-heading (format-time-string
                                (s-join "\n"
                                        `(,(s-concat "OPEN " heading-title)
                                          ":PROPERTIES:"
                                          ":export_file_name: week-ending-%Y%m%d"
                                          ":export_hugo_slug: week-note"
                                          ":END:"
                                          ""
                                          "#+hugo: more"))
                                eow-time)))
            (list heading-title full-heading))
          (format-time-string "%A" (org-current-effective-time)))))

(defun pdc:end-of-week-date (&optional d)
  "Get the gregorian date of the end of the week for the given gregorian date `D'."
  (let* ((d (or d (-> (org-current-effective-time)
                      time-to-days
                      calendar-gregorian-from-absolute)))
         (iso-date (-> d calendar-absolute-from-gregorian
                       calendar-iso-from-absolute))
         (iso-week (nth 0 iso-date))
         (iso-year (nth 2 iso-date))
         (end-of-week-gregorian (-> (list iso-week 7 iso-year)
                                    calendar-iso-to-absolute
                                    calendar-gregorian-from-absolute)))
    end-of-week-gregorian))

(defun pdc:default-weeknote-path ()
  (pdc:weeknote-path
   (calendar-gregorian-from-absolute
    (cond (org-overriding-default-time
           (time-to-days org-overriding-default-time))
          ((or (org-capture-get :time-prompt)
               (equal current-prefix-arg 1))
           (let* ((org-time-was-given nil)
                  (org-end-time-was-given nil)
                  (prompt-time (org-read-date
                                nil t nil "Date for daynote entry:")))
             (org-capture-put
              :default-time
              (if (or org-time-was-given
                      (= (time-to-days prompt-time) (org-today)))
                  prompt-time

                (org-encode-time
                 (apply #'list
                        0 0 org-extend-today-until
                        (cl-cdddr (decode-time prompt-time))))))
             (time-to-days prompt-time)))
          (t (time-to-days (org-current-effective-time)))))))

(defun +org-hugo-find-weeknote-entry (&rest olp)
  "Find or create today in this week's week note."

  ;; This leaves point at the start of the last heading it created.
  (+org-pathbuilder-find-create-path
   nil (-concat olp (pdc:default-weeknote-path)))
  ;; Skip to the end of the subtree
  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-max))))

(defun +org-hugo-find-weeknote-summary (&rest olp)
  (+org-pathbuilder-find-create-path
   nil (-concat olp (-take 2 (pdc:default-weeknote-path))))
  (save-restriction
    (org-narrow-to-subtree)
    (org-goto-first-child)
    (backward-char)
    (delete-region
     (save-excursion (skip-chars-backward " \t\n") (point))
     (point))
    (beginning-of-line)
    (cond ((looking-at (rx "#+hugo: more"))
           (delete-region
            (save-excursion (skip-chars-backward " \t\n") (point))
            (point))
           (insert "\n\n\n\n")
           (backward-char 2))
          (t
           (end-of-line)
           (insert "\n\n")))))

(defun +org-hugo-back-to-article-heading ()
  "Move to the heading of the current article.

Not robust, assumes an article is a direct descendent of a single top level section. Sadly not valid at present."
  (interactive)
  (org-up-heading-all (- (length (org-get-outline-path)) 1)))

(defun +org-hugo-make-default-filename (title)
  "Make a 'safe' file name for a hugo post."
  (require 's)
  (s-concat (format-time-string "%+4Y%m%d") "-" (org-hugo-slug title)))

(defun pdcmacs-hugo-add-properties ()
  "Derive the hugo export file name from the title"
  (interactive)
  (unless (or (org-entry-get (point) "export_file_name" t)
              (org-entry-get (point) "export_hugo_bundle" t))
    (save-excursion
      (+org-hugo-back-to-article-heading)
      (let* ((headline (org-get-heading t t t t)))
        (unless (org-entry-get (point) "export_file_name")
          (org-entry-put (point) "export_file_name"
                         (+org-hugo-make-default-filename headline)))))))

(with-eval-after-load 'org-capture
  (require 'cl-lib)
  (require 's)
  (defun +org-hugo-new-note-post-capture-template ()
    "Returns `org-capture' template string for new Hugo note.
See `org-capture-templates' for more information"
    (let* ((title (read-from-minibuffer)))))

  (add-to-list
   'org-capture-templates
   `("w" "Week Note" plain
     (file+function "~/Sites/bofh.org.uk/org-content/all-posts.org" ,#'(lambda () (+org-hugo-find-weeknote-entry "Week Notes")))
     "%?"
     :empty-lines 1
     :jump-to-captured 1))
  (add-to-list
   'org-capture-templates
   `("W" "Week Summary" plain
     (file+function "~/Sites/bofh.org.uk/org-content/all-posts.org" ,#'(lambda () (+org-hugo-find-weeknote-summary "Week Notes")))
     "%?"
     :empty-lines 1
     :jump-to-captured 1)))


(use-package toml-mode
  :mode
  "\\.toml\\'")

(use-package toml)

(use-package org-pandoc-import
  :straight (org-pandoc-import :host github
                               :repo "tecosaur/org-pandoc-import"
                               :files ("*.el" "filters" "preprocessors")))


(defun pdc-read-toml-frontmatter ()
  "Extract TOML frontmatter from the current .md buffer"
  (interactive)
  (require 'toml)
  (let ((toml-delimiter (rx line-start "+++" line-end)))
    (save-excursion
      (without-restriction
        (goto-char (point-min))
        (unless (looking-at toml-delimiter)
          (error "No starting TOML frontmatter delimiter found."))
        (let ((start) (end))
          (forward-line)
          (setq start (point))
          (unless (re-search-forward toml-delimiter nil t)
            (error "No ending TOML frontmatter delimiter found."))
          (beginning-of-line)
          (setq end (point))
          (with-restriction start end
            (goto-char (point-min))
            (toml:read)))))))

(defun pdc-narrow-to-hugo-body ()
  "Skip over the frontmatter and narrow to the body of current .md buffer"
  (interactive)
  (let ((metadata-matcher (rx (group-n 1 (seq line-start
                                              (| "+++" "---")
                                              line-end))
                              (+? anychar)
                              (backref 1)
                              (+ (*? blank) "\n"))))

    (widen)
    (goto-char (point-min))
    (unless (looking-at metadata-matcher)
      (error "Couldn't find metadata"))
    (narrow-to-region (match-end 0) (point-max))))


(defun pdc-format-hugo-date (time)
  (format-time-string "%Y-%0m-%d" (org-encode-time time)))

(defvar pdc-format-hugo-non-custom-properties
  '("title" "slug" "date" "publishDate" "expiryDate" "draft" "tags" "categories")
  "A list of TOML keys we know what to do with.")

(defun pdc-convert-hugo-md-to-org ()
  (interactive)
  (let* ((frontmatter (pdc-read-toml-frontmatter))
         (in-file (make-temp-file "md-to-org" nil ".md"))
         (out-file (concat (f-no-ext in-file) ".org")))

    (save-excursion
      (save-restriction
        (pdc-narrow-to-hugo-body)
        (write-region (point-min) (point-max) in-file)))
    (org-pandoc-import-to-org nil in-file out-file t)

    (find-file out-file)

    (let* ((title (kva "title" frontmatter))
           (date (kva "date" frontmatter))
           (date (if date (org-parse-time-string date)))
           (publish-date (kva "publishDate" frontmatter))
           (publish-date (if publish-date (org-parse-time-string publish-date)))
           (expiry-date (kva "expiryDate" frontmatter))
           (expiry-date (if expiry-date (org-parse-time-string expiry-date)))
           (tags (append (kva "tags" frontmatter)
                         (--map (concat "@" it) (kva "categories" frontmatter))))
           (slug (or (kva "slug" frontmatter)
                     (org-hugo-slug title)))
           (custom-front-matter
            (--reject
             (member (car it) pdc-format-hugo-non-custom-properties) frontmatter))
           )


      (goto-char (point-min))
      (org-insert-heading)
      (insert title "\n\n")
      (org-back-to-heading)
      (if (kva "draft" frontmatter)
          (org-todo "TODO"))
      (org-set-property "export_date" (pdc-format-hugo-date date))
      (when publish-date
        (org-add-planning-info 'scheduled publish-date t)
        (org-set-property "export_hugo_publishdate"
                          (pdc-format-hugo-date publish-date)))
      (when expiry-date
        (org-set-property "export_hugo_expirydate"
                          (pdc-format-hugo-date expiry-date)))

      (org-set-property "export_file_name" slug)

      (when tags
        (org-set-tags tags))

      (when custom-front-matter
        (require 'tomelr)
        (save-excursion
          ;; Undocumented, useful org-mode function. How rare!
          (org-end-of-meta-data t)
          (insert "\n"
                  "#+begin_src toml :front_matter_extra t\n"
                  (tomelr-encode custom-front-matter) "\n"
                  "#+end_src\n"))))

    ;; Fixup the summary splitter
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (replace-regexp (rx line-start "#+begin_html\n"
                            (* blank) "<!--more-->\n"
                            "#+end_html")
                        "#+hugo: more")))


    (dolist (f (list in-file out-file))
      (delete-file f)))
  )

(provide 'pdcmacs-hugo-support)
