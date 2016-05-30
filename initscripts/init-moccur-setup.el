;; (let ((ad-redefinition-action 'accept))
;;   (req-package color-moccur
;;     :commands (isearch-moccur isearch-all)
;;     :bind ("M-s O" . moccur)
;;     :init
;;     (progn
;;       (bind-key "M-o" 'isearch-moccur isearch-mode-map)
;;       (bind-key "M-O" 'isearch-moccur isearch-mode-map))

;;     :config
;;     (req-package moccur-edit)))
