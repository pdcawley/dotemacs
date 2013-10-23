;;; load-path.el

(defconst user-data-directory
  (expand-file-name "data/" user-emacs-directory))
(defconst user-lisp-directory
  (expand-file-name "elisp/" user-emacs-directory))
(defconst user-lib-directory
  (expand-file-name "lib/" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp/" user-emacs-directory))
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
                    user-site-lisp-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))

(mapc #'add-to-load-path
      (nreverse
       (list
        user-emacs-directory

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

(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(setq load-path (delete-dups load-path))

()
