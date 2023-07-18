;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-hugo-support.el -- Sets up our hugo support using ox-hugo

(eval-when-compile
  (require 'cl-macs))

;;; I really dislike the habit of `defvar'ing private variables, so
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
                                          ":END:"))
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

(defun +org-hugo-find-weeknote-entry (&rest olp)
  "Find or create today in this week's week note."

  ;; This leaves point at the start of the last heading it created.
  (+org-pathbuilder-find-create-path
   nil (-concat olp (pdc:weeknote-path
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
                            (t (time-to-days (org-current-effective-time))))))))
  ;; Skip to the end of the subtree
  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-max))))

(use-package ox-hugo
  :after ox
  :config

  (defun +org-hugo-set-shortcode-props (code &rest props)
    (setf (alist-get code org-hugo-special-block-type-properties)
           props))

  (+org-hugo-set-shortcode-props "newthought" :trim-pre nil :trim-post t)
  (+org-hugo-set-shortcode-props "marginnote" :trim-pre t :trim-post t)

  (defun pdc/wrap-table-in-shortcode (md)
    (if (s-matches? "{{[%<] +table" md)
        md
      (s-wrap md "{{% table %}}\n" "{{% /table %}}")))
  (advice-add 'org-blackfriday-table :filter-return #'pdc/wrap-table-in-shortcode)

  (defun pdc/unfuck-bare-url (url)
    (if (s-matches? "^<.*>$" url)
        (s-replace-regexp "^<\\|>$" "" url)
      url))

  (advice-add 'org-hugo-link :filter-return #'pdc/unfuck-bare-url)


  (defun pdc/org-replace-footnote-with-marginnote (ref)
    (let ((fntext (nth 3 (org-footnote-get-definition ref)))
          (x (org-footnote-at-reference-p)))
      (unless (and x (equalp (car x) ref))
        (org-footnote-goto-previous-reference ref))
      (org-footnote-delete ref)
      (pdc/org-marginnote-new)
      (insert fntext)))

  (defun pdc/convert-footnote-to-marginnote (&optional label)
    "Convert the footnote reference at point to a marginnote"
    (interactive)
    (let ((label (cond
                  (label)
                  ((setq x (org-footnote-at-reference-p))
                   (or (car x)
                       (error "We don't currently handle anonymous footnoetes. TODO")))

                  ((setq x (org-footnote-at-definition-p))
                   (car x))
                  (t (error "Don't know which footnote to remove")))))
      (pdc/org-replace-footnote-with-marginnote label)))

  (defun pdc/wrap-in-shortcode (code)
    "Wrap the point/region with a shortcode, `CODE'."
    (let ((string "")
          (startcode (concat "@@hugo:{{% " code " %}}@@"))
          (endcode (concat "@@hugo:{{% /" code " %}}@@"))
          beg end move)
      (if (org-region-active-p)
          (setq beg (region-beginning)
                end (region-end)
                string (buffer-substring beg end))
        (setq move t))
      (setq string (s-wrap string startcode endcode))
      (when beg (delete-region beg end))
      (insert string)
      (and move (backward-sexp))))

  (defun pdc/org-marginnote-new ()
    "Insert a new marginnote."
    (interactive)
    (pdc/wrap-in-shortcode "marginnote"))

  (defun pdc/org-newthought ()
    "Wrap point/region wiht a newthought shortcode"
    (interactive)
    (pdc/wrap-in-shortcode "newthought"))

  (defun pdc/marginnote-dwim ()
    "Either convert footnote at point to a MN or start a new MN."
    (interactive)
    (cond ((or (org-footnote-at-definition-p)
               (org-footnote-at-reference-p))
           (save-excursion (pdc/convert-footnote-to-marginnote)))
          (t (pdc/org-marginnote-new)))))

;;; Use prodigy to manage hugo server processes


(use-package prodigy
  :commands (prodigy-define-service)
  :general
  (pdcmacs-leader-def :infix "a" "P" 'prodigy)
  :config
  (defvar pdc-hugo-command "hugo")
  (defvar pdc-hugo-server-args
    `("serve"
      "--buildDrafts"
      "--buildFuture"
      "--navigateToChanged"
      "--watch"
      "--environment" "development"
      "--bind" "0.0.0.0"
      "--baseURL" "studio-mini.local"))
  (defun pdc-define-hugo-site (name dir tags &rest args)
    (apply 'prodigy-define-service
           `(:name ,name
                   :command ,pdc-hugo-command
                   :args ,(append pdc-hugo-server-args args)
                   :tags (hugo ,@(-list tags))
                   :cwd ,dir
                   :stop-signal sigkill
                   :kill-process-buffer-on-stop t)))

  (pdc-define-hugo-site "bofh" "~/Sites/bofh.org.uk/" '(blog bofh))
  (pdc-define-hugo-site "st-serve" "~/Sites/singingtogether.co.uk/" '(singing-together st))
  (pdc-define-hugo-site "pdc-serve" "~/Sites/pierscawley.co.uk/" 'pdc))

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
  (unless (org-entry-get (point) "export_file_name" t)
    (save-excursion
      (+org-hugo-back-to-article-heading)
      (let* ((headline (org-get-heading t t t t)))
        (unless (org-entry-get (point) "export_file_name")
          (org-entry-put (point) "export_file_name"
                         (+org-hugo-make-default-filename headline)))))))

(defun +org-hugo--capture-prepare-finalize ()
  (require 'f)
  (let* ((target-file (buffer-file-name (org-capture-get :buffer))))
    (when (and target-file
               (f-ancestor-of? "~/Sites" target-file))
      (pdcmacs-hugo-add-properties))))

(add-hook 'org-capture-prepare-finalize-hook #'+org-hugo--capture-prepare-finalize)

(with-eval-after-load 'org-capture
  (require 'cl-lib)
  (require 's)
  (add-to-list
   'org-capture-templates
   '("b" "bofh.org.uk post" entry (file+headline "~/Sites/bofh.org.uk/org-content/all-posts.org" "Posts")
     "* TODO %?\n\n#+hugo: more\n\n" :jump-to-captured t)
   t #'(lambda (a b) (s-equals? (car a) (car b))))

  (defun +org-hugo-new-note-post-capture-template ()
    "Returns `org-capture' template string for new Hugo note.
See `org-capture-templates' for more information"
    (let* ((title (read-from-minibuffer)))))

  (add-to-list 'org-capture-templates
               '("n" "Note" entry
                 (file+olp+datetree "~/Sites/bofh.org.uk/org-content/all-posts.org" "Notes")
                 "* %U %?\n:properties:\n:export_file_name: nnn.md\n:end:\n"))

  (add-to-list
   'org-capture-templates
   `("w" "Week Note" plain
     (file+function "~/Sites/bofh.org.uk/org-content/all-posts.org" ,#'(lambda () (+org-hugo-find-weeknote-entry "Week Notes")))
     "%?"
     :empty-lines 1
     :jump-to-captured 1)))

(use-package web-mode
  :mode
  "\\.\\(html?\\|json\\|s?css\\)\\'"
  :init
  (setq web-mode-engines-alist
        '(("go" . "/layouts/.*\\.\\(thml?\\|json\\|xml\\|jfw\\)\\'")))

  (setq-default web-mode-markup-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2))

(use-package toml-mode
  :mode
  "\\.toml\\'")


(provide 'pdcmacs-hugo-support)
