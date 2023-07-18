;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-webservice.el -- Support for webservice/api experimentation

(use-package json-mode)

(use-package ox-json
  :after ox)

(use-package org-json
  :after org)

(use-package restclient
  :mode
  "\\`\\*restclient\\*\\'"
  :straight (:type git :host github
             :repo "pashky/restclient.el"
             :fork (:host github
                    :repo "pdcawley/restclient.el")))

(use-package ob-restclient
  :after org
  :straight
  (:type git :host github
   :repo "alf/ob-restclient.el"
   :fork (:host github
          :repo "pdcawley/ob-restclient.el")))

(provide 'pdcmacs-webservice)
