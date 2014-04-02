(defvar prelude-packages
  '(ack-and-a-half align-cljlet anything apache-mode
    bitly
    asciidoc browse-url-dwim buffer-move clojure-mode coffee-mode
    dash dash-functional
    dropbox el-autoyas erefactor
    evil
    f
    fastnav flymake flymake-easy flymake-haml
    flymake-haskell-multi flymake-hlint flymake-perlcritic flymake-sass
    flymake-sass flymake-shell fringe-helper gist git-commit-mode git-gutter
    git-gutter-fringe gitignore-mode gitty go-mode goto-last-change haml-mode
    haskell-mode
    js2-mode json json-mode
    keyfreq less-css-mode lexbind-mode list-utils load-dir
    log4j-mode logito
    markdown-mode mediawiki mo-git-blame
    magit
    multi-term nav nterm nzenburn-theme org org-journal org-magit
    paredit pastebin pcache pcmpl-args perlbrew persistent-soft
    project
    s
    scss-mode string-utils tt-mode typing ucs-utils
    undo-tree unicode-enbox
    winpoint yagist yaml-mode
    zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'prelude-packages)
