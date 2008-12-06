;;; xml-fragment.el --- Minor mode for editing XML fragments w/ nXML

;; Copyright (C) 2006  Marshall T. Vandegrift

;; Author: Marshall T. Vandegrift <llasram@gmail.com>
;; Keywords: xml
;; Version: 0.1

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; XML Fragment Mode is a minor mode for editing XML fragments in nXML
;; mode with validation.  It works by creating in the buffer an
;; invisible and intangible overlay cotaining a header/footer wrapper
;; for the file's XML application.  The mode prevents the point or mark
;; from entering the wrapper, but advises nXML's RELAX NG validator such
;; that it can process the text in the intangible overlay.  The mode
;; omits the wrapper during a normal save of the file.

;;; Installation:

;; To install, just drop this file into a directory in your `load-path'
;; and (optionally) byte-compile it.  To automatically enter XML
;; Fragment Mode when editing XML fragment with nXML, add something
;; like:
;;
;;    (require 'xml-fragment)
;;    (add-hook 'nxml-mode-hook 'xml-fragment-mode-on-maybe)
;;
;; to your .emacs file.

;;; Known Bugs:

;; Auto-save files contain the XML fragment wrapper.

;;; Code:


;; Required libraries

(require 'nxml-mode)
(require 'rng-valid)


;; User definable variables

(defgroup xml-fragment nil
  "Support for editing XML fragments in nXML mode"
  :group 'nxml
  :prefix "xml-fragment-")

(defcustom xml-fragment-wrapper-xhtml1
  '("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
   \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">
  <head>
    <title>!!FRAGMENT</title>
  </head>
  <body>\n"
    .
    "</body>
</html>\n")
  ""
  :group 'xml-fragment)

(defcustom xml-fragment-wrapper-alist
  '(("\\.rhtml$" . xml-fragment-wrapper-xhtml1))
  ""
  :group 'xml-fragment)


;; Mode setup

(defconst xml-fragment-header-comment
  "<!-- xml-fragment-mode begin fragment -->\n"
  "")

(defconst xml-fragment-footer-comment
  "<!-- xml-fragment-mode end fragment -->\n"
  "")

(defvar xml-fragment-overlays nil
  "")

(defun xml-fragment-mode-on-maybe ()
  (interactive)
  (let ((nxml-p (eq major-mode 'nxml-mode))
        (fragment-p (xml-fragment-buffer-fragment-p))
        (wrapper (xml-fragment-wrapper)))
    (when (and nxml-p fragment-p wrapper)
      (xml-fragment-mode 1))))

(define-minor-mode xml-fragment-mode
  "Minor mode to allow validation of XML fragments in nXML mode.
Without ARG, toggle XML Fragment Mode.  With ARG, turn XML
Fragment Mode on iff ARG is positive and off otherwise."
  nil " Frag" nil
  (when mmm-major-mode-hook
    (add-hook 'mmm-major-mode-hook
              'xml-fragment-twiddle-overlays
              'append))
  (if xml-fragment-mode
      (let ((nxml-p (eq major-mode 'nxml-mode))
            (fragment-p (xml-fragment-buffer-fragment-p))
            (wrapper (xml-fragment-wrapper)))
        (if (and nxml-p fragment-p wrapper)
            (xml-fragment-mode-setup)
          (xml-fragment-mode -1)
          (cond
           ((not nxml-p)
            (error "XML Fragment Mode activated on non-nXML Mode buffer."))
           ((not fragment-p)
            (error "XML Fragment Mode activated on non-fragment buffer."))
           ((not wrapper)
            (error "XML Fragment Mode unknown XML fragment type.")))))
    (xml-fragment-mode-teardown)))

(defun xml-fragment-buffer-fragment-p ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (not (or (search-forward "<!DOCTYPE" (point-at-eol) t)
               (search-forward "<?xml" (point-at-eol) t))))))

(defun xml-fragment-mode-setup ()
  (add-to-invisibility-spec 'xml-fragment)
  (xml-fragment-make-overlays)
  (add-hook 'post-command-hook 'xml-fragment-avoid-wrapper nil t)
  (add-hook 'kill-buffer-hook 'xml-fragment-delete-overlays nil t)
  (add-hook 'write-contents-hooks 'xml-fragment-delete-overlays 'append)
  (add-hook 'change-major-mode-hook 'xml-fragment-delete-overlays nil t)
  (add-hook 'after-save-hook 'xml-fragment-make-overlays nil t))

(defun xml-fragment-mode-teardown ()
  (xml-fragment-twiddle-overlays)
  (xml-fragment-delete-overlays)
  (remove-hook 'post-command-hook 'xml-fragment-avoid-wrapper t)
  (remove-hook 'kill-buffer-hook 'xml-fragment-delete-overlays t)
  (remove-hook 'write-contents-hooks 'xml-fragment-delete-overlays)
  (remove-hook 'change-major-mode-hook 'xml-fragment-delete-overlays t)
  (remove-hook 'after-save-hook 'xml-fragment-make-overlays t))

(defun xml-fragment-twiddle-overlays ()
  (when xml-fragment-mode
    (xml-fragment-delete-overlays)
    (xml-fragment-make-overlays)))
  
(defun xml-fragment-make-overlays ()
  (unless xml-fragment-overlays
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t)
          (buffer-undo-list t))
      (unwind-protect
          (save-excursion
            ;; Create overlays with header and footer fragments
            (let* ((regions (xml-fragment-insert-wrapper))
                   (header (car regions)) (footer (cadr regions))
                   (header-begin (car header)) (header-end (cadr header))
                   (footer-begin (car footer)) (footer-end (cadr footer)))
              (set (make-local-variable 'xml-fragment-overlays)
                   (list (make-overlay header-begin header-end nil nil nil)
                         (make-overlay footer-begin footer-end nil t t))))
            ;; Setup overlay properties
            (dolist (overlay xml-fragment-overlays)
              (progn
                (overlay-put overlay 'intangible 'xml-fragment)
                (overlay-put overlay 'invisible  'xml-fragment)))
            ;; Make the text read-only so no one else stomps on it
            (let ((header (car xml-fragment-overlays))
                  (footer (cadr xml-fragment-overlays)))
              (set-text-properties (overlay-start header)
                                   (overlay-end header)
                                   '(read-only xml-fragment
                                               front-sticky (read-only)
                                               rear-nonsticky (read-only)))
              (set-text-properties (overlay-start footer)
                                   (overlay-end footer)
                                   '(read-only xml-fragment))))
        (set-buffer-modified-p modified)))))

(defun xml-fragment-insert-wrapper ()
  (let ((xml-fragment-wrapper (xml-fragment-wrapper)))
    (list (list (goto-char (point-min))
                (progn (insert (car xml-fragment-wrapper)
                               xml-fragment-header-comment)
                       (point)))
          (list (goto-char (point-max))
                (progn (insert xml-fragment-footer-comment
                               (cdr xml-fragment-wrapper))
                       (point))))))

(defun xml-fragment-wrapper ()
  (let (result (alist xml-fragment-wrapper-alist))
    (while (and alist (not result))
      (let ((entry (car alist)))
        (when (string-match (car entry) (or (buffer-file-name) (buffer-name)))
          (let ((value (cdr entry)))
            (setq result (if (symbolp value) (symbol-value value) value)))))
      (setq alist (cdr alist)))
    result))

(defun xml-fragment-delete-overlays ()
  (when xml-fragment-overlays
    (save-excursion
      (let ((modified (buffer-modified-p))
            (inhibit-read-only t)
            (buffer-undo-list t))
        (unwind-protect
            (progn
              (dolist (overlay xml-fragment-overlays)
                (delete-region (overlay-start overlay) (overlay-end overlay))
                (delete-overlay overlay))
              (setq xml-fragment-overlays nil))
          (set-buffer-modified-p modified))))))

(defun xml-fragment-avoid-wrapper ()
  (let ((header (car xml-fragment-overlays))
        (footer (cadr xml-fragment-overlays)))
    (cond ((bobp) (goto-char (overlay-end header)))
          ((eobp) (goto-char (overlay-start footer))))
    (when mark-active
      (cond ((= (mark) (point-min)) (set-mark (overlay-end header)))
            ((= (mark) (point-max)) (set-mark (overlay-start footer)))))))


;; Advice for nXML RELAX NG validator

(defadvice rng-validate-while-idle
  (around xml-fragment-allow-motion-rng-validate activate)
  (let ((inhibit-point-motion-hooks t))
    ad-do-it))

(defadvice rng-validate-quick-while-idle
  (around xml-fragment-allow-motion-rng-validate-quick activate)
  (let ((inhibit-point-motion-hooks t))
    ad-do-it))

(defadvice rng-auto-set-schema
  (around xml-fragment-allow-motion-rng-auto-set-schema activate)
  (let ((inhibit-point-motion-hooks t))
    ad-do-it))

(provide 'xml-fragment)

;;; xml-fragment.el ends here
