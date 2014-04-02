;;; load-path.el

(setq pdc/load-path-before load-path)

(defconst user-data-directory
  (expand-file-name "data/" user-emacs-directory))
(defconst user-lisp-directory
  (expand-file-name "elisp/" user-emacs-directory))
(defconst user-lib-directory
  (expand-file-name "lib/" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory))
(defconst user-external-lisp-directory
  (expand-file-name "elisp/external/" user-emacs-directory))
(defconst user-override-directory
  (expand-file-name "override/" user-emacs-directory))
(defconst user-initscripts-directory
  (expand-file-name "initscripts/" user-emacs-directory))

(defun add-to-load-path (path &optional dir)
  (add-to-list 'load-path
               (expand-file-name path (or dir user-emacs-directory))))

(dolist (dir (nreverse
              (list user-override-directory
                    user-lisp-directory
                    user-lib-directory
                    user-site-lisp-directory
                    user-external-lisp-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))


(mapc #'add-to-load-path
      (nreverse
       (list

        "ruby/"
        "misc/"
        "Emacs/"
        "html5-el/"
        "coffee-mode/"
        "twittering-mode/"
        "yasnippet/"
        "gnus/lisp/"
        "shime/"
        "elisp/"
        "/usr/local/share/emacs/site-lisp"
        "/usr/local/opt/git/share/git-core/contrib/emacs/")))

(require 'package)
(package-initialize)
(require 'ert)
(require 'use-package)

(setq load-path
      (delete-dups
       (delete
	(file-name-as-directory (expand-file-name user-emacs-directory))
	(mapcar (lambda (dir)
		  (file-name-as-directory
		   (expand-file-name dir)))
		load-path))))

()
