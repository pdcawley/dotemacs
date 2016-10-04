;; -*- lexical-binding: t -*-

(require 'use-package)

(bindings|define-prefix buffer "b" "buffers")
(bindings|define-prefix org "o")
(bindings|define-prefix toggle "t" "toggles")

(defvar mode-leader-key
  "M-,"
  "Prefix for mode-leader")

(general-define-key mode-leader-key '(nil :which-key "mode-leader"))

(general-create-definer pdc|with-leader
                        :prefix leader-key :keymaps 'global)
(general-create-definer pdc|with-mode-leader
                        :prefix mode-leader-key)

(defun pdc/mplist-get (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

(defun bindings//expand-add-toggle (name props)
  (let* ((wrapper-func (intern (format "toggle|%s"
                                       (symbol-name name))))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (status (plist-get props :status))
         (condition (plist-get props :if))
         (doc (plist-get props :documentation))
         (on-body (pdc/mplist-get props :on))
         (off-body (pdc/mplist-get props :off))
         (keymaps (pdc/mplist-get props :keymaps))
         (binding (plist-get props :toggle-keys))
         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status)))
    `(progn
       (defun ,wrapper-func ()
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive)
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (if ,status-eval
                 (progn ,@off-body
                        (message ,(format "%s disabled." name)))
               ,@on-body
               (message ,(format "%s enabled." name)))
           (message "This toggle is not supported.")))
       ,@(when binding
           `((pdc|with-leader ,@(if keymaps `(:keymaps ',keymaps))
                              ,binding '(,wrapper-func :which-key
                                          ,(or doc (symbol-name name))))))
       ,@(when status
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on" (symbol-name name))
               (interactive)
               (unless ,status-eval (,wrapper-func)))
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when ,status-eval (,wrapper-func))))))))

(defmacro bindings|add-toggle (name &rest props)
  (declare (indent 1))
  (bindings//expand-add-toggle name props))

(defmacro pdc|general-bind-hydra
    (name leader &rest specs &key no-cancel &allow-other-keys)
  "Bind a hydra in such a way that others can share the prefix."
  (declare (indent defun))
  (let ((hydra-key (intern (format "hydra-%s" (symbol-name name)))))
    `(progn
       (defhydra ,hydra-key (nil nil :color red)
         ,(symbol-name name)
         ,@specs
         ,@(unless no-cancel '(("q" nil "cancel" :color blue))))
       (dolist (it ',specs)
         (pcase it
           (`(,key ,fn ,desc . ,(pred (lambda (props)
                                        (plist-get props :exit))))
            (let ((keys (concat ,leader key)))
              (general-define-key :prefix leader-key
                keys (list fn :which-key desc))))
           (`(,key ,fn ,desc . ,(and props
                                     (pred (lambda (props)
                                             (not (plist-get props :exit))))))
            (let ((hydra-fn (plist-get props :cmd-name))
                  (keys (concat ,leader key)))
              (general-define-key :prefix leader-key
                keys (list hydra-fn :which-key desc)))))))))
;; (put 'pdc|general-bind-hydra 'lisp-indent-function 2)

(provide 'init-leaders)
