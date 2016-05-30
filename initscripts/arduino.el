(req-package arduino-mode
  :mode ("\\.\\(pde\\|\\ino\\)$" . arduino-mode)
  :commands arduino-mode
  :init
  (setq ede-arduino-appdir
  "/Applications/Arduino.app/Contents/Resources/Java"))
