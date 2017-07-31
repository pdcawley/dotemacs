(eval-when-compile (require 'req-package))

(req-package multiple-cursors
  :general
  ("s-<mouse-1>" 'mc/add-cursor-on-click)
  (:keymaps
   ctl-x-map
   "C-m" 'mc/mark-all-dwim
   "<return>" 'pdc/execute-extended-command-short)
  :config
  (with-eval-after-load 'init-leaders
    (pdc|general-bind-hydra mc "m"
      ("<" mc/mark-previous-like-this)
      (">" mc/mark-next-like-this )
      ("M-<" mc/unmark-previous-like-this)
      ("M->" mc/unmark-next-like-this)
      ("i" mc/insert-numbers)
      ;; ("h" mc/hide-unmatched-lines-mode)
      ("d" mc/mark-all-symbols-like-this-in-defun :color blue)
      ("r" mc/reverse-regions)
      ("s" mc/sort-regions))
    (pdc|with-leader
     "m C-a" 'mc/edit-beginnings-of-lines
     "m A" 'mc/mark-all-like-this
     "m a" 'mc/mark-all-dwim
     "m C-e" 'mc/edit-ends-of-lines
     "m e" 'mc/edit-lines)
    )
  )
"


(provide 'init-cursor)"
