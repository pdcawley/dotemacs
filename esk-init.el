;;; init.el --- start to bootstrap our loadup system
;;
;; Lifted from the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

(add-hook 'after-init-hook
 `(lambda ()
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    (org-babel-load-file (expand-file-name "init.org" user-emacs-directory))))

;;; init.el ends here
