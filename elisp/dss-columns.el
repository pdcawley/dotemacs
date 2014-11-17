(require 'column-marker)
(defvar  dss/major-column-face 'dss/major-column-face
  "major column grid marker")
(defface dss/major-column-face '((t (:background "#484848")))
  "major column grid marker"
  :group 'faces)

(defvar  dss/minor-column-face 'dss/minor-column-face
  "minor column grid marker")
(defface dss/minor-column-face '((t (:background "#2c2c2c")))
  "minor column grid marker"
  :group 'faces)

(column-marker-create dss/column-marker-1 dss/minor-column-face)
(column-marker-create dss/column-marker-2 dss/major-column-face)
(column-marker-create dss/column-marker-3 dss/minor-column-face)
(column-marker-create dss/column-marker-4 dss/major-column-face)
(column-marker-create dss/column-marker-5 dss/minor-column-face)
(column-marker-create dss/column-marker-6 dss/major-column-face)
(column-marker-create dss/column-marker-7 dss/minor-column-face)
(column-marker-create dss/column-marker-8 dss/major-column-face)

(defun dss/column-grid ()
  (interactive)
  ;; col-highlight-face
  (hl-line-mode -1)
  (dss/column-marker-1 10)
  (dss/column-marker-2 20)
  (dss/column-marker-3 30)
  (dss/column-marker-4 40)
  (dss/column-marker-5 50)
  (dss/column-marker-6 60)
  (dss/column-marker-7 70)
  (dss/column-marker-8 80))

(defun dss/column-grid-off ()
  (interactive)
  (dss/column-marker-1 -1)
  (hl-line-mode t))
