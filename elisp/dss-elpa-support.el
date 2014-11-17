;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.



;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

;; (eval-after-load "package"
;;   '(progn
;;      (package-initialize)
;;      ;; (when (> emacs-major-version 23)
;;      ;;   (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/")))
;;      ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;      ))

(provide 'dss-elpa-support)
