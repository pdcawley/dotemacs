(req-package web-mode
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.tpl.php\\'" . web-mode)
   ("\\.jsp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.html?\\'" . web-mode))
  :config
  (defun pdc/web-mode-hook ()
    "Hooks for web-mode"
    (setq web-mode-markup-indent-offset 2)))

(req-package css-mode
  :mode ("\\.css\\'" . css-mode))
