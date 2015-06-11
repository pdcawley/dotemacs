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
      el-get-sources '(
                             ;; (:name yasnippet
                             ;;        :website "https://github.com/capitaomorte/yasnippet.git"
                             ;;        :description "YASnippet is a template system for Emacs."
                             ;;        :type github
                             ;;        :pkgname "capitaomorte/yasnippet"
                             ;;        :features "yasnippet"
                             ;;        :compile "yasnippet.el")
                             ;; (:name yasnippet
                             ;;        :website "http://code.google.com/p/yasnippet/"
                             ;;        :description "YASnippet is a template system for Emacs."
                             ;;        :type git
                             ;;        :url "https://github.com/capitaomorte/yasnippet.git"
                             ;;        :features "yasnippet"
                             ;;        :prepare (lambda ()
                             ;;                   ;; Set up the default snippets directory
                             ;;                   ;;
                             ;;                   ;; Principle: don't override any user settings
                             ;;                   ;; for yas/snippet-dirs, whether those were made
                             ;;                   ;; with setq or customize.  If the user doesn't
                             ;;                   ;; want the default snippets, she shouldn't get
                             ;;                   ;; them!
                             ;;                   (unless (or (boundp 'yas/snippet-dirs) (get 'yas/snippet-dirs 'customized-value))
                             ;;                     (setq yas/snippet-dirs
                             ;;                           (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets")))))

                             ;;        :post-init (lambda ()
                             ;;                     ;; Trick customize into believing the standard
                             ;;                     ;; value includes the default snippets.
                             ;;                     ;; yasnippet would probably do this itself,
                             ;;                     ;; except that it doesn't include an
                             ;;                     ;; installation procedure that sets up the
                             ;;                     ;; snippets directory, and thus doesn't know
                             ;;                     ;; where those snippets will be installed.  See
                             ;;                     ;; http://code.google.com/p/yasnippet/issues/detail?id=179
                             ;;                     (put 'yas/snippet-dirs 'standard-value
                             ;;                          ;; as cus-edit.el specifies, "a cons-cell
                             ;;                          ;; whose car evaluates to the standard
                             ;;                          ;; value"
                             ;;                          (list (list 'quote
                             ;;                                      (list (concat el-get-dir (file-name-as-directory "yasnippet") "snippets"))))))
                             ;;        ;; byte-compile load vc-svn and that fails
                             ;;        ;; see https://github.com/dimitri/el-get/issues/200
                             ;;        :compile nil)
                             ;; (:name wanderlust :type git
                             ;;        :url "https://github.com/wanderlust/wanderlust.git"
                             ;;        :load-path ("site-lisp/wl" "elmo")
                             ;;        :build (mapcar
                             ;;                (lambda (target-and-dirs)
                             ;;                  (list el-get-emacs
                             ;;                        (mapcar (lambda (pkg)
                             ;;                                  (mapcar (lambda (d) `("-L" ,d)) (el-get-load-path pkg)))
                             ;;                                (append
                             ;;                                 '("apel" "flim" "semi")
                             ;;                                 (when (el-get-package-exists-p "bbdb") (list "bbdb"))))
                             ;;                        "--eval" (prin1-to-string
                             ;;                                  '(progn (setq wl-install-utils t)
                             ;;                                          (setq wl-info-lang "en")
                             ;;                                          (setq wl-news-lang "en")))

                             ;;                        (split-string "-batch -q -no-site-file -l WL-MK -f")
                             ;;                        target-and-dirs))
                             ;;                '(("wl-texinfo-format" "doc")
                             ;;                  ("compile-wl-package"  "site-lisp" "icons")
                             ;;                  ("install-wl-package" "site-lisp" "icons")))
                             ;;        :info "doc/wl.info")
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


                             ;; (:name slime-fuzzy :type http :url
                             ;;        "http://elder-gods.org/~larry/repos/slime-tracker/contrib/slime-fuzzy.el")

                                        ;(:name slime :type elpa)

                             ;; (:name slime
                             ;;        :description "Superior Lisp Interaction Mode for Emacs"
                             ;;        :type git
                             ;;        :module "slime"
                             ;;        :info "doc"
                             ;;                                 ;:url "https://github.com/nablaone/slime.git"
                             ;;        :url "git://sbcl.boinkor.net/slime.git"
                             ;;        :load-path ("." "contrib")
                             ;;        :compile (".")
                             ;;        :build ("make -C doc && rm contrib/slime-tramp.elc")
                             ;;        )

                             (:name workgroups :type git :url "https://github.com/tlh/workgroups.el.git")
                             (:name monky :type git
                                    :url "https://github.com/ananthakumaran/monky.git"
                                    :build ("make all"))
                             ;; (:name magit
                             ;;        :website "https://github.com/magit/magit#readme"
                             ;;        :description "It's Magit! An Emacs mode for Git."
                             ;;        :type git
                             ;;        :url "http://github.com/magit/magit.git"
                             ;;        :branch "maint"
                             ;;        :info "."
                             ;;        ;; that used to be how to build it :build ("./autogen.sh" "./configure" "make")
                             ;;        :build ("make all")
                             ;;        :build/darwin `(,(concat "PATH=" invocation-directory ":$PATH make all")))

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
                             (:name desktop-recover :type git :url "https://github.com/doomvox/desktop-recover.git")
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
                             (:name lineker :type http
                                    :url "http://www.helsinki.fi/~sjpaavol/programs/lineker.el")
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
                             (:name etags-select :type emacswiki)
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
                             ;; (:name haskell-mode
                             ;;        :description "A Haskell editing mode"
                             ;;        :type git
                             ;;        :url "https://github.com/haskell/haskell-mode.git"
                             ;;        ;; :load "haskell-site-file.el"
                             ;;        )
                             )
      ;; (--each extra-el-get-sources
      ;;   (cl-pushnew it el-get-sources
      ;;               :test (lambda (a b)
      ;;                       (equal (el-get-source-name a)
      ;;                              (el-get-source-name b)))))
)



(setq dss-el-get-packages
      '(ac-dabbrev
        ;; ac-slime
        ace-jump-mode
        ack-and-a-half
        align-cljlet
        anything
        apache-mode
        asciidoc
        auto-complete
        auto-complete-etags
        auto-complete-extension
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
        desktop-recover
        diminish
        el-autoyas
        elein
        emacs-jabber
        ess
        etags-select
        evil
        evil-surround
        f
        fastnav
        filladapt
        flymake
        flymake-coffee
        flymake-easy
        flymake-haml
        flymake-node-jshint
        flymake-sass
        flymake-shell
        fm
        fringe-helper
        gist
        git-commit-mode
        git-gutter
        git-gutter-fringe
        ;; go-mode
        goto-last-change
        haml-mode
        haskell-mode
        helm
        highlight-symbol
        hlinum
        idle-highlight-mode
        iedit
        isearch+
        jquery-doc
        js2-mode
        json
        json-mode
        keyfreq
        less-css-mode
        ;; lexbind-mode
        lineker
        list-register
        list-utils
        logito
        magit
        monky
        mark-multiple
        markdown-mode
        mediawiki
        mo-git-blame
        monky
        moz
        multi-term
        nav
        nterm
        nzenburn-theme
        org
        org-journal
        org-magit
        org-mode
        paredit
        ;; pastebin
        pbcopy
        pcache
        pcmpl-args
        pcre2el
        perlbrew
        persistent-soft
        pg
        popup
        popup-kill-ring
        pos-tip
        pos-tip ;; required by popup-kill-ring
        powerline
        pretty-mode
        project
        protbuf
        rainbow-delimiters
        rainbow-mode
        rect+
        restclient
        s
        s-buffer
        sackspace
        scss-mode
        session
        highlight-chars
        ;; slime
        ;; slime-js
        smex
        ssh-config-mode
        string-inflection
        string-utils
        sunrise-commander
        sunrise-x-checkpoints
        sunrise-x-modeline
        swift-mode
        tabulated-list
        tagedit
        thing-cmds
        thingatpt+
        transpose-frame
        trie
        tt-mode
        typing
        ucs-utils
        unbound
        undo-tree
        unfill
        unicode-enbox
        unipoint
        virtualenv
        virtualenv ;;pylookup
        visible-mark
        visual-regexp
        vline
        whitespace-cleanup-mode
        whole-line-or-region
        window-numbering
        winpoint
        workgroups
        yagist
        yaml-mode
        zencoding-mode))

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

;;;
