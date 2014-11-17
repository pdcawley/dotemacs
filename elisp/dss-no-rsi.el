(setq type-break-interval (* 20 60)
      type-break-mode-line-message-mode t
      type-break-keystroke-threshold '(1 . 8000)
      type-break-good-rest-interval (* 5 10))
;(type-break-mode)
; I plan on adding mode and set of hooks for activating/deactivating things like type-break-mode when I'm online
; This mode will also force me to be clocked in to an org entry any time that I'm on the computer.
(provide 'dss-no-rsi)
