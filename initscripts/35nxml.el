;; 35nxml.el -- configuration for nxml-mode
(eval-after-load
    "rng-loc"
  '(add-to-list 'rng-schema-locating-files (concat dotfiles-dir "/html5-el/schemas.xml")))

;; 35nxml.el ends
