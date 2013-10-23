;;; 40cedet.el --- Initial configuration for CEDET and ECB

;; CEDET
;(setq semantic-load-turn-useful-things-on t)
;(require 'cedet)
;(require 'semantic)
;(require 'semantic-wisent)
;(require 'semanticdb)
                                        ;(semantic-load-enable-code-helpers)
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
;(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;(semantic-mode 1)
;(global-ede-mode nil)
;(setq ede-arduino-appdir "/Applications/Arduino.app/Contents/Resources/Java")

(use-package arduino-mode
  :mode ("\\.\\(pde\\|\\ino\\)$" . arduino-mode)
  :commands arduino-mode
  :init
  (setq ede-arduino-appdir
  "/Applications/Arduino.app/Contents/Resources/Java"))



;; ECB
;(require 'ecb)
