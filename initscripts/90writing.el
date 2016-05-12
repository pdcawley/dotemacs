(use-package artbollocks-mode
  :ensure t
  :init
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverate") t) "\\b"))
    (setq artbollocks-jargon nil)))
