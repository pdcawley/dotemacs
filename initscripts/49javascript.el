;;; 49javascript.el --- Custom javascript-mode configuration

;(require 'javascript-mode)
;(require 'js-mode)
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
