(use-package ack
  :ensure t
  :bind
  (("C-. s" . ack))
  :init
  (progn
    (defun pdc-ack-default-directory (arg)
      "A function for `ack-default-directory-function`.
With no \\[universal-argument], find the project root directory;
with one \\[universal-arguement], return `default-directory`;
otherwise, interactively choose a directory"
      (ack-default-directory (cond ((not arg) 4)
                                   ((= (prefix-numeric-value arg) 4) nil)
                                   (t arg))))
    (setq ack-default-directory-function 'pdc-ack-default-directory)
    (setq ack-and-a-half-executable (expand-file-name "~/bin/ack"))))
