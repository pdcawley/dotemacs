;;; bootstrap
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer (url-retrieve-synchronously
                        "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)
    (load-library "el-get")))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes/")
(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes/")

;;; see https://github.com/purcell/emacs.d for more ideas

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (el-get-elpa-build-local-recipes)
;;(require 'dash)
;;; load packages
(setq el-get-byte-compile t
      el-get-use-autoloads t
      el-get-sources
      '((:name desktop-recover :type git :url "https://github.com/doomvox/desktop-recover.git")
        (:name lineker :type http
               :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")
        (:name etags-select :type emacswiki)

        )
      old-el-get-sources '(
                       (:name gist :type git :url "https://github.com/tels7ar/gist.el")
                       (:name auto-complete
                              :website "http://cx4a.org/software/auto-complete/"
                              :description "The most intelligent auto-completion extension."
                              :type git
                              :url "http://github.com/auto-complete/auto-complete.git"
                              :load-path "."
                              :post-init (progn
                                           (require 'auto-complete)
                                           (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                                           ;; the elc is buggy for some reason
                                           (let ((f "~/.emacs.d/el-get/auto-complete/auto-complete-config.elc"))
                                             (if (file-exists-p f)
                                                 (delete-file f)))
                                           (require 'auto-complete-config)
                                           (ac-config-default)
                                           ))

                       (:name workgroups :type git :url "https://github.com/tlh/workgroups.el.git")
                       (:name monky :type git
                              :url "https://github.com/ananthakumaran/monky.git"
                              :build ("make all"))
                       (:name isearch+ :type emacswiki)
                       (:name linkd :type emacswiki)
                       (:name grep-a-lot :type git :url
                              "https://github.com/ZungBang/emacs-grep-a-lot")
                       (:name idle-highlight-mode :type git :url
                              "https://github.com/nonsequitur/idle-highlight-mode.git")
                       (:name list-register :type http :url "http://www.bookshelf.jp/elc/list-register.el")
                       (:name bm :type http :url "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")
                       (:name window-numbering :type git :url "https://github.com/nschum/window-numbering.el.git")
                       (:name column-marker :type http
                              :url "http://www.emacswiki.org/emacs/download/column-marker.el")
                       (:name col-highlight :type emacswiki)
                       (:name eredis :type http :url "http://eredis.googlecode.com/svn/trunk/eredis.el")
                       (:name ac-dabbrev :type emacswiki)
                       (:name cobol-mode :type emacswiki)
                       ;;
                       (:name show-wspace :type emacswiki)
                       (:name visible-mark :type emacswiki)
                       (:name transpose-frame :type emacswiki)
                       (:name relax :type git :url "https://github.com/technomancy/relax.el")
                       (:name moz :type git :url "http://github.com/bard/mozrepl.git"
                              :load "chrome/content/moz.el")
                       (:name rainbow-delimiters :type git :url "https://github.com/jlr/rainbow-delimiters.git")
                       (:name rainbow-mode
                              :type git
                              :url "https://github.com/emacsmirror/rainbow-mode.git")
                       (:name js2-mode :type git :url "https://github.com/mooz/js2-mode")
                       (:name jquery-doc :type git :url "git://github.com/ananthakumaran/jquery-doc.el.git")

                       (:name flymake-coffee :type git :url "git://github.com/purcell/flymake-coffee.git")
                       (:name flymake-node-jshint :type git :url "git://github.com/jegbjerg/flymake-node-jshint.git")
                       (:name elein :type git :url "https://github.com/remvee/elein.git")
                       (:name pg :type http :url "http://www.online-marketwatch.com/pgel/pg.el")
                       (:name stompem :type git :url "https://github.com/jwhitlark/Stompem.git")


                       (:name elnode :type git :url "https://github.com/tavisrudd/elnode.git")
                       (:name gh :type git :url "https://github.com/sigma/gh.el.git")
                       (:name pcache :type git :url "https://github.com/sigma/pcache.git")
                       (:name auto-complete-etags :type git :url "https://github.com/whitypig/auto-complete-etags.git")

                       (:name restclient :type git :url "https://github.com/pashky/restclient.el.git")
                       (:name buster-mode :type git :url "git://gitorious.org/buster/buster-mode.git")
                       (:name mark-multiple :type git :url "git://github.com/magnars/mark-multiple.el.git")
                       (:name aws-el :type git :url "https://github.com/ieure/aws-el.git")

                       (:name helm :type git :url "https://github.com/emacs-helm/helm.git")

                       (:name evil-surround
                              :type git
                              :url "https://github.com/timcharper/evil-surround.git")
                       (:name clojure-mode :type git :url "https://github.com/tavisrudd/clojure-mode.git")
      ))



(setq dss-el-get-packages
      '(
        ;; ac-dabbrev
        ;; ac-slime

        ido-vertical-mode
        ido-ubiquitous
        ido-hacks
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
        color-theme
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
        highlight-cl
        hlinum
        ibuffer-vc
        idle-highlight-mode
        iedit
        isearch+
        jquery-doc
        js2-mode
        json
        json-mode
        keyfreq
        kv
        less-css-mode
        lexbind-mode
        lineker
        list-register
        list-utils
        logito
        magit
        magit-gitflow
        monky
        mark-multiple
        markdown-mode
        mediawiki
        mo-git-blame
        monky
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
        per-window-point
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
        protbuf
        rainbow-delimiters
        rainbow-mode
        restclient
        s
        scss-mode
        session
        highlight-chars
        slime
        smex
        ssh-config
        string-utils
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
        whitespace
        whole-line-or-region
        window-numbering
        winpoint
        workgroups
        yagist
        yaml-mode
        zencoding-mode
        ))

(unless (require 'filladapt nil t)
  (add-to-list 'dss-el-get-packages 'filladapt))

(defun el-get-update-all ()
  "Update all el-get packages
  This was copied from https://github.com/purcell/emacs.d/blob/master/init-el-get.el"
  (interactive)
  (dolist (package dss-el-get-packages)
    (unless (memq (plist-get (el-get-package-def package) :type) '(http-tar elpa))
      (el-get-update package))))

(let ((el-get-verbose t))
  (el-get 'sync dss-el-get-packages))

(add-to-list 'load-path "~/.emacs.d/vendor")

(provide 'pdc-packages-and-paths)
;;;
