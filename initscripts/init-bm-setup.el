(req-package bm
  :requires hydra
  :init
  (with-eval-after-load 'hydra
    (defhydra hydra-bm (nil nil :color red)
      "bm"
      ("b" bm-last-in-previous-buffer "previous buffer")
      ("f" bm-first-in-next-buffer "next buffer")
      ("g" bm-previous "previous")
      ("l" bm-show-all "show all")
      ("c" bm-toggle "toggle")
      ("m" bm-toggle "toggle")
      ("n" bm-next "next")
      ("p" bm-previous "previous"))
    (pdc|with-leader
     "c" '(hydra-bm/body :which-key "bm"))))

