;;; see https://github.com/purcell/emacs.d for more ideas

(require 'package)
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(package-refresh-contents)
;; (el-get-elpa-build-local-recipes)
;;(require 'dash)
;;; load packages

(setq pdc-packages
      '(
        ac-dabbrev
        ac-slime
        counsel

        ;; ido-vertical-mode
        ;; ido-ubiquitous
        ;; ido-hacks
        projectile
        geiser
        web-mode
        elisp-slime-nav
        redshank
        yasnippet
        yasnippets
        yasnippet-snippets
        ace-jump-mode
        ack-and-a-half
        align-cljlet
        anything
        apache-mode
        asciidoc
        auto-complete
        auto-complete-etags
        auto-complete-extension
        auto-async-byte-compile
        autopair
        bm
        breadcrumb
        browse-kill-ring
        buffer-move
        clojure-mode
        coffee-mode
        col-highlight
        column-marker
        dash
        dedicated
        desktop-recover
        diminish
        el-autoyas
        elein
        emacs-jabber
       ;; ess
       ;; etags-select
        evil
        evil-surround
        exec-path-from-shell
        f
        fastnav
        filladapt
        flycheck
        elixir-mode
        flymake
        flymake-coffee
        flymake-easy
        flymake-haml
        flymake-sass
        flymake-shell
        ;; fm
        fringe-helper
        gist
        git-gutter
        git-gutter-fringe
        go-mode
        goto-last-change
        haml-mode
        haskell-mode
        helm
        hide-comnt
        highlight-symbol
        hlinum
        ibuffer-vc
        idle-highlight-mode
        iedit
        jquery-doc
        js2-mode
        json
        json-mode
        keyfreq
        kv
        less-css-mode
        lineker
        list-register
        list-utils
        logito
        magit
        magit-gitflow
        monky
        multiple-cursors
        markdown-mode
        mediawiki
        mo-git-blame
        multi-term
        nav
        nterm
        org-magit
        org-mode
        paredit
        pastebin
        pbcopy
        pcache
        pcre2el
        persistent-soft
    pointback
        pg
        popup
        popup-kill-ring
        pos-tip
        powerline
        pretty-mode
        project-buffer-mode
        project-explorer
        project-local-variables
        project-mode
        project-root
        protobuf-mode
        rainbow-delimiters
        rainbow-mode
        restclient
        s
        scss-mode
        session
        highlight-chars
        ;;slime
        smex
        ssh-config
        string-utils
        swiper
        swift-mode
        tabulated-list
        tagedit
        thing-cmds
        thingatpt+
        transpose-frame
        ucs-utils
        unbound
        undo-tree
        unicode-enbox
        use-package
        visible-mark
        visual-regexp
        vline
        whitespace-cleanup-mo
        whole-line-or-region
        window-numbering
        winpoint
        workgroups
        yagist
        yaml-mode
        zencoding-mode
        ))

(unless (require 'filladapt nil t)
  (add-to-list 'pdc-packages 'filladapt))

(defun pdc-fetch-new-packages ()
  "Fetch any packages added to pdc-packages"
  (interactive)
  (dolist (package pdc-packages)
    (unless (package-installed-p package)
      (ignore-errors (package-install package)))))

(defun pdc-update-all-packages ()
  "Update all installed packages"
  (interactive)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (condition-case nil
    (package-menu-execute t)
      (error
       (package-menu-execute)))))

(provide 'pdc-packages-and-paths)
;;;
