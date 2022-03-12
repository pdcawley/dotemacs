;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading the newest compiled .el file
(setq load-prefer-newer noninteractive)

(setq package-enable-at-startup nil)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence the warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make it happen asynchronously
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

