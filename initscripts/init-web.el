(require 'req-package)

(req-package httprepl
  :require s dash
  :commands httprepl)

(req-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(req-package elnode
  :commands (elnode-stop elnode-start))

(req-package peek-mode
  :commands peek-mode
  :require elnode
  :config (elnode-start 'peek-mode-dispatcher-handle :port 8008 :host "localhost"))

(provide 'init-web)
