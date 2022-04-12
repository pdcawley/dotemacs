;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-hugo-support.el -- Sets up our hugo support using ox-hugo

(use-package ox-hugo
  :after ox
  :config
  (push '("b" "BOFH Post" entry (file "~/Sites/bofh.org.uk/org-content/all-posts.org")))

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
      "--bind" "0.0.0.0"
      "--baseURL" ,(system-name)))
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
  (save-excursion
    (+org-hugo-back-to-article-heading)
    (let* ((headline (org-get-heading t t t t)))
      (unless (org-entry-get (point) "export_file_name")
        (org-entry-put (point) "export_file_name"
                       (+org-hugo-make-default-filename headline))))))

(defun +org-hugo--capture-prepare-finalize ()
  (require 'f)
  (let* ((target-file (buffer-file-name (org-capture-get :buffer))))
    (when (and target-file
               (f-ancestor-of? "~/Sites" target-file))
      (pdcmacs-hugo-add-properties))))

(add-hook 'org-capture-prepare-finalize-hook #'+org-hugo--capture-prepare-finalize)

(eval-after-load 'org-capture
  (lambda ()
    (require 'cl-lib)
    (require 's)
    (cl-pushnew '("b" "bofh.org.uk post" entry (file+headline "~/Sites/bofh.org.uk/org-content/all-posts.org" "Posts")
                  "* TODO %?\n\n" :jump-to-captured t)
                org-capture-templates
                :test (lambda (a b) (s-equals? (car a) (car b))))))

(provide 'pdcmacs-hugo-support)
