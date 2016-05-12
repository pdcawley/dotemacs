(require 'pretty-mode)

(defun pretty-add-patterns! (patterns)
  "Add to pretty-patterns in a convenient way

PATTERNS should be of the form ((GLYPH (REGEXP MODE ...) ...)
...). as per `pretty-compile-patterns`. GLYPH should be a
character. MODE should be the name of a major mode without the
\"-mode\". Converts these into `pretty-patterns` style patterns
and merges them with the current `pretty-patterns`"
  (let ((orig-pretty-patterns pretty-patterns))
    (setq pretty-patterns
          (let (working-patterns (copy-tree orig-pretty-patterns))
            (loop for (glyph . pairs) in patterns do
                  (loop for (regexp . major-modes) in pairs do
                        (loop for mode in major-modes do
                              (let* ((mode (intern (concat (symbol-name mode)
                                                           "-mode")))
                                     (assoc-pair (assoc mode working-patterns))
                                     (entry (cons regexp glyph)))
                                (if assoc-pair
                                    (push entry (cdr assoc-pair))
                                  (push (cons mode (list entry))
                                        working-patterns))))))
            working-patterns))))



;;(setq my-pretties pretty-patterns)
