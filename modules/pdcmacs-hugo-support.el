;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-hugo-support.el -- Sets up our hugo support using ox-hugo

(eval-when-compile
  (require 'cl-macs))



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
