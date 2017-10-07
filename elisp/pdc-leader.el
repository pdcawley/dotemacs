;;; pdc-leader.el -- Support for setting up leader keys -*- lexical-binding: t -*-
;;; Commentary:
;;; Loads up all the stuff we need for our leader key based keymaps

;;; Code:
(eval-when-compile (require 'use-package))

(use-package ht :ensure t)
(use-package s :ensure t)
(use-package kv :ensure t)
(use-package diminish :ensure t)

(defvar leader-map (make-sparse-keymap))
(defvar leader-key "M-m")

(defvar mode-leader-key "M-,"
  "Prefix for mode specific leader.")

(use-package which-key
  :ensure t
  :diminish " Ⓚ"
  :commands (which-key-mode)
  :init
  (which-key-mode)

  :config
  (let ((new-descriptions
         '(("select-window-\\"0-9 "\\)" . "window \\1")
           ("avy-goto-word-or-subword-1" . "avy»word")
           ("shell-command" . "shell cmd")
           ("universal-argument" . "universal arg")
           ("er/expand-region" . "expand region"))))
    (dolist (nd new-descriptions)
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cdr nd))
            which-key-replacement-alist)))
  (setq which-key-special-keys nil
        which-key-echo-keystrokes 0.02
        which-key-max-description-length 32
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.4))

(defun bindings//expand-define-prefix (desc key docstr)
  (let* ((descname (symbol-name desc))
         (leader-variable-name (intern (format "%s-leader-key" descname)))
         (infix-variable-name (intern (format "%s-infix-key" descname)))
         (prefix-variable-name-deprecated (intern (format "%s-prefix-key" descname)))
         (doc (or docstr (symbol-name desc))))
    `(progn
       (defvar ,leader-variable-name (format "%s %s" leader-key ,key)
         ,(format "leader-key + prefix for %s" doc))
       (defvar ,infix-variable-name ,key
         ,(format "infix for %s - use with `pdc|leader :infix %s' type commands" doc infix-variable-name))
       (defvar ,prefix-variable-name-deprecated ,infix-variable-name
         ,(format "Obsolete copy of %s" infix-variable-name))
       (make-obsolete ',prefix-variable-name-deprecated ',infix-variable-name "2017-08-27")
       (which-key-add-key-based-replacements ,leader-variable-name ,doc))))

(defmacro bindings|define-prefix (desc key &optional docstr)
  (declare (indent 1))
  (bindings//expand-define-prefix desc key docstr))

(defmacro bindings|define-prefix-1 (descvar key docstr)
  (declare (indent 1))
  `(eval (bindings-expand-define-prefix `,,descvar ,key ,docstr)))



(use-package hydra :ensure t
  :commands defhydra
  :init
  (setq hydra-head-format "%s → "))

(use-package general
  :ensure t)

(defvar bindings--prefixes-hash (ht)
  "Holds the map of prefix descriptions to their keys.")

(defun bindings//declare-prefix (key-seq description)
  "Register a prefix KEY-SEQ and its DESCRIPTION.

The description is used as the key of `bindings--prefixes-hash' as well
as any subdescriptions obtained by spliting on ``/''.  Any keys that have
have no spaces in them are also registered as keywords via
`kvthing->keyword'"
  (let* ((description (s-chop-suffix "/" (s-chop-prefix "/" description)))
         (shortdescs (when (s-contains? "/" description)
                       (s-split "/" description t)))
         (full-leader (concat leader-key " " key-seq)))
    (which-key-add-key-based-replacements full-leader description)
    ;; (ht-set! bindings--prefixes-hash description key-seq)
    ;; (unless (s-countains? " " description)
    ;;   (ht-set! bindings--prefixes-hash (kvthing->keyword description)))
    (dolist (desc (cons description shortdescs))
      (unless (s-contains? " " desc)
        (ht-set! bindings--prefixes-hash (kvthing->keyword desc)
                 key-seq))
      (ht-set! bindings--prefixes-hash desc key-seq)))  )

(defun bindings//prefix (name &optional keys)
  "Return the prefix associated with NAME.

If KEYS is supplied, append it to the prefix.  Raise an error if there is no
such prefix."
  (let* ((prefix (if (ht-contains? bindings--prefixes-hash name)
                     (ht-get bindings--prefixes-hash name)
                   (error "No such leader %S" name)))
         (prefix (if keys
                     (concat prefix " " keys)
                   prefix)))
    prefix))

(defun bindings//leader (name &optional keys)
  "Return the leader sequence associated with NAME.
Append KEYS to the result, if supplied"
  (concat leader-key " " (bindings//prefix name keys)))

(defun bindings//list->keys (list &optional lookupfn)
  (let ((lookupfn (or lookupfn #'bindings//prefix)))
    (pcase list
      (`(,(or (pred stringp) (pred keywordp)) . ,_)
       (apply lookupfn list))
      (_ list))))

(defmacro pdc|with-leader (&rest args)
  (declare (indent defun))
  (let ((parsed-args
         (cl-loop for (key value) on args by 'cddr
                  nconc (pcase (list key value)
                          (`(:prefix ,(pred symbolp))
                           (list key (bindings//list->keys (list value) 'bindings//leader)))
                          (`(:prefix ,_)
                           (list key (bindings//list->keys value 'bindings//leader)))
                          (`(:infix ,(pred symbolp))
                           (list key (bindings//list->keys (list value))))
                          (`(:infix ,_)
                           (list key (bindings//list->keys value)))
                          (`(,(pred keywordp) ,_)
                           (list key value))
                          (`(,_ ,_)
                           (list (bindings//list->keys key) value))))))
    `(pdc/with-leader ,@parsed-args)))

(general-define-key mode-leader-key '(nil :which-key "mode-leader"))
(general-define-key leader-key)

(general-create-definer pdc/with-leader
                        :prefix leader-key :keymaps 'global)
(put 'pdc/with-leader 'lisp-indent-function 'defun)

(general-create-definer pdc|with-mode-leader
                        :prefix mode-leader-key)
(put 'pdc|with-mode-leader 'lisp-indent-function 'defun)

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

(defvar bindings-toggles '()
  "List of all declared toggles.
The strucutre of an element is a property list (name :func FUNCTION :doc STRING :key STRING).")

(defun bindings//expand-add-toggle (name props)
)


(defmacro bindings|add-toggle (name &rest props)
  (declare (indent 1))
  (let* ((docstr (if (stringp (car props))
                     (pop props)))
         (mode (plist-get props :mode) )
         (wrapper-func (intern (format "toggle|%s"
                                       (symbol-name name))))
         (wrapper-func-on (intern (format "%s-on" wrapper-func)))
         (wrapper-func-off (intern (format "%s-off" wrapper-func)))
         (wrapper-func-status (intern (format "%s-status" wrapper-func)))
         (status (or (plist-get props :status) mode name))
         (doc (or (plist-get props :documentation)
                  docstr))
         (on-body (if mode `((,mode)) (pdc/mplist-get props :on)))
         (off-body (if mode `((,mode -1)) (pdc/mplist-get props :off)))
         (prefix-arg-var (plist-get props :prefix))
         (on-message (plist-get props :on-message))
         (condition (plist-get props :if))

         (status-eval `(and (or (and (symbolp ',status) (boundp ',status))
                                (listp ',status))
                            ,status))
         ;; If we're defined by `define-minor-mode this will toggle
         ;; the mode
         (toggle-body (or (pdc/mplist-get props :toggler)
                          (if (or on-body off-body)
                              `(if ,status-eval
                                   (progn ,@off-body)
                                 ,@on-body)
                            `(,name 'toggle))))
         (keymaps (pdc/mplist-get props :keymaps))
         (binding (plist-get props :toggle-keys)))
    `(progn

       (push (append '(,name)
                     '(:function ,wrapper-func
                                 :predicate ,wrapper-func-status)
                     ',props)
             bindings-toggles)

       ;; toggle function
       (defun ,wrapper-func ,(if prefix-arg-var (list prefix-arg-var) ())
         ,(format "Toggle %s on and off." (symbol-name name))
         (interactive ,@(when prefix-arg-var (list prefix-arg-var)))
         (if (or (null ',condition)
                 (and (or (and (symbolp ',condition) (boundp ',condition))
                          (listp ',condition))
                      ,condition))
             (progn
               ,toggle-body
               ,(if on-message
                    `(message (if ,status-eval on-message (format "%s disabled." ,(symbol-name name))))
                  `(message (let ((enabled ,status-eval))
                              (format "%s %s." (quote ,name) (if ,status-eval "enabled" "disabled"))))))
           (message "This toggle is not supported")))

       (defun ,wrapper-func-status ()
         ,status-eval)

       ,@(when status
           `((defun ,wrapper-func-on ()
               ,(format "Toggle %s on" (symbol-name name))
               (interactive)
               (unless ,status-eval (,wrapper-func)))
             (defun ,wrapper-func-off ()
               ,(format "Toggle %s off." (symbol-name name))
               (interactive)
               (when ,status-eval (,wrapper-func)))))
       ,@(when binding
           `((pdc|with-leader ,@(if keymaps `(:keymaps ',keymaps))
                              ,binding '(,wrapper-func :which-key
                                                       ,(or doc (symbol-name name)))))))))

(defun pdc//shortdoc (fn)
  (when-let* ((doc (documentation fn)))
    (substring doc 0 (string-match "\n" doc))))

(defun pdc//bind-hydra-spec (spec leader)
  (pcase spec
    (`(,key ,fn ,desc . ,(pred (lambda (props)
                                 (plist-get props :exit))))
     (let ((keys (concat leader " " key)))
       (general-define-key :prefix leader-key
                           keys (list fn :which-key desc))))
    (`(,key ,_ ,desc . ,(and props
                             (pred (lambda (props)
                                     (not (plist-get props :exit))))))
     (let ((hydra-fn (plist-get props :cmd-name))
           (long-leader (concat leader-key " " leader)))
       (general-define-key :prefix long-leader
                           key (list hydra-fn :which-key desc))))))


(defmacro pdc|general-bind-hydra
    (name leader &key no-cancel &allow-other-keys &rest specs)
  "Bind the hydra NAME in such a way that others can share the LEADER."
  (declare (indent defun))
  (cl-flet
      ((canonicalize-spec (spec)
                          (let* ((key (car spec))
                                 (action (cadr spec))
                                 (tail (cddr spec))
                                 (caption (cond ((stringp (car tail))
                                                 (pop tail))
                                                ((symbolp action)
                                                 (symbol-name action))
                                                ((functionp action)
                                                 (pdc//shortdoc action))
                                                (t "??"))))
                            `(,key ,action ,caption ,@tail
                                   :cmd-name ,(intern
                                               (format "hydra-%s/%s" name action))))))
    (let ((hydra-key (intern (format "hydra-%s" (symbol-name name))))
          (specs (-map #'canonicalize-spec specs)))
      `(progn
         (defhydra ,hydra-key (nil nil :color red)
           ,(symbol-name name)
           ,@specs
           ,@ (unless no-cancel '(("q" nil "cancel" :color blue))))
         (--each ',specs
           (pdc//bind-hydra-spec it ,leader))))))

(put 'pdc|general-bind-hydra 'lisp-indent-function 2)

(provide 'pdc-leader)
