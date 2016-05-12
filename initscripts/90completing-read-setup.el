;;; We use a combination of ubiquitous IDO, ibuffer and smex to handle various
;;; 'completing read' situations. So... set 'em all up here.

(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper)
   ;;("M-x" . counsel-M-x)
)
  ;; :config
  ;; (counsel-mode t)
  )

