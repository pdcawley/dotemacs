;;; 00setup.el --- Personal initial setup code

;; Copyright (c) 2006 Marshall T. Vandegrift

;;; Commentary:

;; This file contains miscellaneous startup code which needs to / can
;; run early-on

;; TODO: make this generic

(add-to-list 'load-path (concat dotfiles-dir "ruby/"))
(add-to-list 'load-path (concat dotfiles-dir "misc/"))
(add-to-list 'load-path (concat dotfiles-dir "Emacs/"))
(add-to-list 'load-path (concat dotfiles-dir "html5-el/"))
(add-to-list 'load-path (concat dotfiles-dir "coffee-mode/"))
(add-to-list 'load-path (concat dotfiles-dir "twittering-mode/"))
(add-to-list 'load-path (concat dotfiles-dir "yasnippet/"))
(add-to-list 'load-path (concat dotfiles-dir "gnus/lisp/"))
(add-to-list 'load-path (concat dotfiles-dir "shime/"))
(add-to-list 'load-path (concat dotfiles-dir "emacs-color-theme-solarized/"))

;; Make sure we have font-lock to start withco
(require 'font-lock)
(require 'zenburn)
(color-theme-zenburn)
(global-hl-line-mode t)

;; Just say no to splash screens
(setq inhibit-startup-message t)

;; Abbrevs
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; I like UTF-8
(prefer-coding-system 'utf-8)

;; Disable advanced featres? Bah.
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Some minor modes I usually enjoy
(column-number-mode t)

;; Load up a bunch of common libraries
(require 'tramp)
(require 'ffap)
(require 'url)
(require 'saveplace)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)



;; RELAX-NG editing
(require 'rnc-mode)
(update-auto-mode-binding '("\\.rnc\\'" . rnc-mode))

;; SVN
(require 'vc)
(require 'vc-git)

(add-to-list 'load-path (concat dotfiles-dir "/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/yasnippet/snippets"))
(setq yas/window-system-popup-function
      'yas/x-popup-menu-for-template)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'align)

(setq pdc:elisp-external-dir
      (expand-file-name "elisp/external" dotfiles-dir))

(dolist (project (directory-files pdc:elisp-external-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; Utility functions etc...

(defun region-or-thing (thing)
  "Return a vector containing the region and its bounds if there is one
or the thing at the point and its bounds if there is no region"
  (if (use-region-p)
      (vector (buffer-substring-no-properties (region-beginning) (region-end))
              (region-beginning) (region-end))
    (let* ((bounds (bounds-of-thing-at-point thing))
           (beg (car bounds))
           (end (cdr bounds)))
      (vector (buffer-substring-no-properties beg end) beg end))))

(server-start)

(defun put-alist (key value alist)
  "Set cdr of an element (KEY . ...) in ALIST to VALUE and return ALIST.
If there is no such element, create a new pair (KEY . VALUE) and
return a new alist whose car is the new pair and cdr is ALIST."
  (let ((elm (assoc key alist)))
    (if elm
        (progn
          (setcdr elm value)
          alist)
      (cons (cons key value) alist))))

(defun update-tramp-emacs-server-ssh-port-forward ()
  "Update TRAMP's ssh method to forward the Emacs server port to the local host.
This lets emacsclient on the remote host open files in the local Emacs server.

put-alist, used below, is defined in alist, which is part of the APEL library:
http://kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/index.en.html"
  (let* ((ssh-method (assoc "pdcssh" tramp-methods))
         (ssh-args (cadr (assoc 'tramp-login-args ssh-method))))
    (put-alist 'tramp-login-args
               (list (put-alist "-R" (let ((port (process-contact server-process :service)))
                                       ;; put-alist makes a dotted pair for the key/value, but tramp-methods
                                       ;; needs a normal list, so put the value inside a list so that the
                                       ;; second part of the dotted pair (ie the cdr) is a list, which converts
                                       ;; it from a dotted pair into a normal list.
                                       (list (format "%d:127.0.0.1:%d" port port)))
                                ssh-args))
               ssh-method)))

(defadvice server-process-filter (before handle-remote-emacsclient-file activate)
  "Detect remote emacsclient and inject the tramp '/host:' prefix.

  Note the hack here that assumes remote emacsclient invocations
  have the regex '-tty /dev/(pts/[0-9]|ttype)[0-9]+)' in their
  command sequence string, and all others have either no -tty or
  a one-digit /dev/pts/.... I haven't yet found a better way to
  distinguish local and remote clients."
  (if (string-match "-tty /dev/\\(pts/[0-9]\\|ttyp\\)[0-9]+" (ad-get-arg 1))
      (with-parsed-tramp-file-name default-directory parsed
        (let* ((message (ad-get-arg 1))
               (absolute (and (string-match "-file \\([^ ]+\\)" message)
                              (file-name-absolute-p (match-string 1 message))))
               (tramp-prefix (tramp-make-tramp-file-name parsed-method
                                                         parsed-user
                                                         parsed-host
                                                         nil))
               (dir (if absolute nil parsed-localname)))
          (ad-set-arg 1 (replace-regexp-in-string "-file "
                                                  (concat "-file " tramp-prefix dir) message))))))

(defun ssh-shell (host bufname)
  "SSH to a remote host in a shell-mode buffer using TRAMP."
  (update-tramp-emacs-server-ssh-port-forward)
  (let ((default-directory (format "/%s:" host))
        (tramp-remote-process-environment
         (cons (format "EDITOR='emacsclient -f ~/.emacs.d/%s_server'" (getenv "HOST"))
               tramp-remote-process-environment)))
    (shell bufname))
  ;; copy emacs server file so remote emacsclient can connect to this emacs
  (let ((default-directory "/tmp")
        (local-server-file (process-get server-process :server-file))
        (remote-server-file (format "~/.emacs.d/%s_server" (getenv "HOST"))))
    (async-shell-command
     (format "scp -v %s %s:%s" local-server-file host remote-server-file))))

;; end 00setup.el
