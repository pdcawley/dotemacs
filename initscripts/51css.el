;; ;;; 51css.el --- Custom css-mode configuration

;; (setq cssm-newline-before-closing-bracket t)
;; (setq cssm-indent-function #'cssm-c-style-indenter)

;; ;; Some eval-on-load stuff
;; (require 'css-mode)

;; (defadvice cssm-complete-property
;;   (after cssm-complete-add-space activate)
;;   "Modify CSS completion to add a space after full completion."
;;   (when (eq (char-before) ?:) (insert " ")))

;; (define-skeleton cssm-insert-semicolon
;;   "Inserts a semicolon." nil
;;   ";" "\n" >)

;; (defadvice cssm-enter-mirror-mode
;;   (after cssm-enter-mirror-semicolon activate)
;;   "Add electric semicolons to css-mode's \"mirror mode.\""
;;   (define-key cssm-mode-map (read-kbd-macro ";")  'cssm-insert-semicolon))

;; (defadvice cssm-leave-mirror-mode
;;   (after cssm-leave-mirror-semicolon activate)
;;   "Add electric semicolons to css-mode's \"mirror mode.\""
;;   (define-key cssm-mode-map (read-kbd-macro ";")  'self-insert-command))

