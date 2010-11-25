;; 35nxml.el -- configuration for nxml-mode
(eval-after-load
    "rng-loc"
  '(add-to-list 'rng-schema-locating-files (concat dotfiles-dir "/html5-el/schemas.xml")))

(require 'whattf-dt)
(load "nxhtml/autostart")

(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;; 35nxml.el ends
