;;; idea from http://github.com/technomancy/emacs-starter-kit/blob/master/init.el
(setq dss-hostname (car (split-string system-name "\\.")))
(defvar dss-system-specific-config (concat dss-dotfiles-dir dss-hostname ".el"))
(defvar dss-user-specific-config (concat dss-dotfiles-dir user-login-name ".el"))
(defvar dss-user-specific-dir (concat dss-dotfiles-dir user-login-name "/"))
(add-to-list 'load-path dss-user-specific-dir)

(if (file-exists-p dss-system-specific-config) (load dss-system-specific-config))
(if (file-exists-p dss-user-specific-config) (load dss-user-specific-config))
(if (file-exists-p (concat dss-user-specific-dir "init.el"))
    (load (concat dss-user-specific-dir "init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-user-specific-init)
