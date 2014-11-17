(defvar dss/ecb-loaded nil)
(defun dss/load-ecb ()
  (interactive)
  (require 'ecb)

  (setq ecb-source-file-regexps (quote ((".*" ("\\(^\\(\\.\\|#\\)\\|\\(~$\\|\\(_i\\|_p\\)\\.\\(h\\|c\\)$\\)\\|\\(~$\\|\\.\\(elc\\|obj\\|o\\|class\\|lib\\|pyc\\|a\\|so\\|cache\\|bak\\|vspscc\\|WW\\|plg\\|tlb\\|opt\\|ncb\\|scc\\|aps\\|APS\\|clw\\|bin\\)$\\)\\)") ("^\\.\\(emacs\\|gnus\\)$")))))

  ;(setq ecb-compilation-buffer-names (quote (("*Calculator*") ("*vc*") ("*vc-diff*") ("*shell*") ("\\*[cC]ompilation.*\\*" . t) ("\\*i?grep.*\\*" . t) ("*Backtrace*") ("*Compile-log*") ("*bsh*") ("^\\*Whitespace.+\\*" . t) ("*Process List*") ("*Python Output*") ("*Directory"))))
  (setq ecb-compilation-buffer-names (quote ()))
  ; ("*Help*") ("*Completions*") ("*Apropos*") ("*Occur*")
  ;(setq ecb-compilation-major-modes (quote (compilation-mode grep-mode completion-mode erc-mode gud-mode change-log-mode)))
  (setq ecb-compilation-major-modes (quote ()))

  (add-hook 'ecb-activate-before-layout-draw-hook (lambda () (ecb-toggle-compile-window 0)))
  (add-hook 'ecb-activate-hook (lambda () (ecb-toggle-compile-window 0)))

  (ecb-layout-define "dss/leftright" left-right nil
    (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
    (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (ecb-split-ver 0.5 t)
    (if (fboundp (quote ecb-set-sources-buffer)) (ecb-set-sources-buffer) (ecb-set-default-ecb-buffer))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (if (fboundp (quote ecb-set-directories-buffer)) (ecb-set-directories-buffer) (ecb-set-default-ecb-buffer))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (ecb-split-ver 0.5 t)
    (if (fboundp (quote ecb-set-directories-buffer)) (ecb-set-directories-buffer) (ecb-set-default-ecb-buffer))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
    )
  (setq dss/ecb-loaded t)
  (smex-update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-ecb)
