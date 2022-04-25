;;; core-modules.el -- Loads up the rest of our core libs -*- lexical-binding: t; -*-

(defvar pdc-init-modules-p nil)

(defvaralias 'pdc-after-init-modules-hook 'after-init-hook)

;;
;;; Bootstrap the API

(defun pdc-initialize-core-modules ()
  "Load our core files for an interactive session"
  (require 'core-keybinds nil t)
  (require 'core-projects nil t)
  (require 'core-editor nil t))

(defun pdc-intialize-modules (&optional force-p no-config-p)
  "Initializes our API and sets up some helpful hooks.
Noops if used more than once unless FORCE-P is non-nil"
  (when (or force-p (not pdc-init-modules-p))
    (setq pdc-init-modules-p t)
    (unless no-config-p
      (pdc-initialize-core-modules))
    (when custom-file
      (load custom-file 'noerror nil))))

(provide 'core-modules)
;;; core-modules.el ends here
