(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-arguments (quote ("--nopager")))
 '(ack-and-a-half-executable "/u/virtual/pdc/bin/ack" t)
 '(ack-and-a-half-prompt-for-directory t)
 '(ack-prompt-for-directory t)
 '(auto-save-interval 300)
 '(bitly-access-token "7148af4682ed05084192d937d9e9ada306a316bd")
 '(blink-cursor-mode nil)
 '(blink-matching-paren-distance 51200)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(browse-url-generic-program "google-chrome")
 '(buffers-menu-show-directories (quote unless-uniquify))
 '(coffee-command "/usr/local/share/npm/bin/coffee")
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(comint-buffer-maximum-size 20000)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-prompt-read-only t)
 '(comint-scroll-to-bottom-on-input t)
 '(compilation-error-screen-columns nil)
 '(completion-category-overrides (quote ((buffer (styles basic substring initials)))))
 '(completion-styles (quote (basic partial-completion initials emacs22)))
 '(confluence-save-credentials t)
 '(confluence-url "https://confluence.ntt.eu/rpc/xmlrpc")
 '(cperl-auto-newline nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-fix-hanging-brace-when-indent nil)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-indent-subs-specially nil)
 '(cperl-label-offset -4)
 '(cperl-merge-trailing-else nil)
 '(cperl-tab-always-indent t)
 '(cua-enable-cua-keys nil)
 '(cua-keep-region-after-copy t)
 '(cua-remap-control-v nil)
 '(current-language-environment "English")
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (pdc-zenburn)))
 '(custom-safe-themes
   (quote
    ("66bd972acaaffbe24ac94c6a81e71c33d7ff87746c5e08c51920005a8113fe53" "92f744d9a97bb8f99565fade6bf093787466e22e6d1be4b0cf68e07c73f93f19" "5359fb73898840ada2ae5d69877f2d817a1672185013049955672fc6438a4bd4" "83cffe882af5fdb996897f17b68cf2b309ca618afc0fba0eae3884bfee890429" "2a954a51b071b4e63d62acf6381f2ddbea9a528c94360d797f82319a78a2c57b" "4c9ba94db23a0a3dea88ee80f41d9478c151b07cb6640b33bfc38be7c2415cc4" default)))
 '(default-fill-column 78 t)
 '(default-frame-alist
    (quote
     ((top . 50)
      (left . 100)
      (width . 110)
      (height . 40)
      (cursor-color . "#dcdccc"))))
 '(default-input-method nil)
 '(default-mime-charset (quote utf-8))
 '(delete-selection-mode nil nil (delsel))
 '(dictionary-default-popup-strategy "soundex")
 '(diff-switches "-u")
 '(display-buffer-reuse-frames nil)
 '(ecb-options-version "2.33beta1")
 '(enable-recursive-minibuffers t)
 '(enable-remote-dir-locals t)
 '(ewd-kp-usage (quote num))
 '(fill-column 78)
 '(font-lock-support-mode (quote jit-lock-mode))
 '(footnote-section-tag-regexp "Footnotes\\(\\[.\\]\\)?: *")
 '(footnote-use-message-mode nil)
 '(frame-background-mode (quote light))
 '(geiser-guile-binary "/usr/local/bin/guile")
 '(geiser-racket-binary "/Applications/Racket/bin/racket")
 '(global-auto-revert-mode t)
 '(global-mark-ring-max 60)
 '(global-whitespace-mode nil)
 '(gnus-select-method
   (quote
    (nnimap "thermeon"
            (nnimap-server-port 993)
            (nnimap-authenticator
             (quote login))
            (nnimap-expunge-on-close never)
            (nnimap-stream ssl)
            (nnimap-address "eu-mail.car-rental-world.com")
            (nnimap-user "pdc"))))
 '(grep-command "grep -n -e")
 '(haskell-program-name "ghci")
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-abbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill)))
 '(history-delete-duplicates t)
 '(icomplete-mode nil)
 '(indent-region-mode t t)
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(inferior-lisp-program "/usr/local/bin/openmcl -K utf-8")
 '(initial-major-mode (quote lisp-interaction-mode))
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-program-name "/usr/local/bin/aspell")
 '(ispell-silently-savep t)
 '(iswitchb-default-method (quote samewindow))
 '(jabber-account-list
   (quote
    (("piers.cawley@headforwards.com"
      (:network-server . "xmpp.atlasit.com")))))
 '(jabber-alert-info-message-hooks (quote (jabber-info-display)))
 '(jabber-message-alert-same-buffer nil)
 '(jabber-mode-line-mode t)
 '(jabber-muc-autojoin
   (quote
    ("ntteodevroom@conference.atlasit.com" "ehportal@conference.atlasit.com")))
 '(jabber-muc-default-nicknames
   (quote
    (("ntteodevroom@conference.atlasit.com" . "Piers Cawley")
     ("ehportal@conference.atlasit.com" . "Piers Cawley"))))
 '(javascript-indent-level 2)
 '(js2-auto-indent-flag nil)
 '(js2-indent-on-enter-key nil)
 '(js2-language-version 150)
 '(js2-missing-semi-one-line-override t)
 '(js2-strict-inconsistent-return-warning nil)
 '(less-css-compile-at-save nil)
 '(mac-emulate-three-button-mouse nil t)
 '(mac-inline-input-method-mode nil t)
 '(mac-key-mode t)
 '(mac-key-mode-hook nil)
 '(mac-wheel-button-is-mouse-2 t t)
 '(magit-emacsclient-executable "emacsclient")
 '(magit-git-executable "/u/virtual/pdc/bin/git")
 '(magit-gitk-executable "gitk")
 '(max-specpdl-size 10000)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(minibuffer-eldef-shorten-default t)
 '(mode-line-position
   (quote
    ((line-number-mode
      ((column-number-mode
        (10
         #("(%03l,%03c)" 0 9
           (help-echo "Line number and Column number
mouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
(keymap
 (mode-line keymap
            (down-mouse-1 keymap
                          (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                              (:toggle . column-number-mode))
                          (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                            (:toggle . line-number-mode))
                          "Toggle Line and Column Number Display"))))))
        (6
         #(" L%l" 0 4
           (help-echo "Line Number
mouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
(keymap
 (mode-line keymap
            (down-mouse-1 keymap
                          (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                              (:toggle . column-number-mode))
                          (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                            (:toggle . line-number-mode))
                          "Toggle Line and Column Number Display"))))))))
      ((column-number-mode
        (5
         #(" C%c" 0 4
           (help-echo "Column number
mouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
(keymap
 (mode-line keymap
            (down-mouse-1 keymap
                          (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                              (:toggle . column-number-mode))
                          (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                            (:toggle . line-number-mode))
                          "Toggle Line and Column Number Display")))))))))
     "["
     (-3
      #("%p" 0 2
        (help-echo "Size indication mode
mouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
(keymap
 (mode-line keymap
            (down-mouse-1 keymap
                          (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                              (:toggle . column-number-mode))
                          (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                            (:toggle . line-number-mode))
                          "Toggle Line and Column Number Display"))))))
     (size-indication-mode
      #("/%I" 0 3
        (help-echo "Size indication mode
mouse-1: Display Line and Column Mode Menu" mouse-face mode-line-highlight local-map
(keymap
 (mode-line keymap
            (down-mouse-1 keymap
                          (column-number-mode menu-item "Display Column Numbers" column-number-mode :help "Toggle displaying column numbers in the mode-line" :button
                                              (:toggle . column-number-mode))
                          (line-number-mode menu-item "Display Line Numbers" line-number-mode :help "Toggle displaying line numbers in the mode-line" :button
                                            (:toggle . line-number-mode))
                          "Toggle Line and Column Number Display"))))))
     "]")) t)
 '(mode-require-final-newline (quote visit-save))
 '(mouse-region-delete-keys (quote ([delete] [deletechar] [backspace])))
 '(next-line-add-newlines nil)
 '(next-screen-context-lines 1)
 '(ns-alternate-modifier (quote meta))
 '(ns-command-modifier (quote super))
 '(ns-function-modifier (quote alt))
 '(ns-right-alternate-modifier (quote alt))
 '(ns-use-native-fullscreen nil)
 '(ns-use-qd-smoothing nil)
 '(ns-use-system-highlight-color nil t)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
(quote
 ("~/Dropbox/org/codex.org" "~/Dropbox/org/blog.org" "~/Dropbox/org/todo.org" "~/Dropbox/org/technology.org" "~/Dropbox/org/journal.org")))
 '(org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
 '(org-modules
(quote
 (org-bbdb org-bibtex org-docview org-gnus org-id org-info org-protocol org-eshell org-jsinfo org-habit org-inlinetask org-irc org-mac-message org-mew org-mhe org-protocol org-rmail org-vm org-wl org-w3m org-mouse org-mac-iCal org-timer)))
 '(org-protocol-default-template-key "w")
 '(org-startup-indented t)
 '(org-timer-default-timer 25)
 '(org-todo-keywords
(quote
 ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)" "WONTFIX(W@/!)")
  (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELLED(c@/!)")
  (sequence "OPEN(O)" "|" "CLOSED(C)"))))
 '(overflow-newline-into-fringe t)
 '(pc-selection-mode nil)
 '(pdc/mc-cmds
(quote
 (forward-sexp backward-sexp cperl-electric-semi cperl-electric-brace cperl-electric-lbrace cperl-electric-backspace cperl-electric-paren cperl-electric-rparen)))
 '(rails-always-use-text-menus t)
 '(read-mail-command (quote gnus))
 '(require-final-newline t)
 '(safe-local-variable-values
(quote
 ((magit-git-executable . "/home/staff/pdc/git/bin/git")
  (cperl-close-paren-offset . -1)
  (cperl-brace-imaginary-offset . 0)
  (cperl-tab-always-indent . t)
  (cperl-indent-parens-as-block . t)
  (cperl-fix-hanging-brace-when-indent)
  (cperl-close-paren-offset . 0)
  (cperl-brace-imaginary-offset . 4)
  (cperl-auto-newline)
  (ack-and-a-half-executable . "/home/staff/pdc/bin/ack")
  (ffap-perl-module-path "/scpc:grebe:/home/piers.cawley/Projects/nexus/lib/perl" "/scpc:grebe:/home/piers.cawley/Projects/nexus/gui/nboss-customer-portal" "/scpc:grebe:/home/piers.cawley/Projects/nexus/gui/customer-portal" "/scpc:grebe:/home/piers.cawley/Projects/nexus/gui/partner-portal" "/scpc:grebe:/home/piers.cawley/Projects/nexus/gui/sir")
  (ffap-perl-module-path mapcar
                         (function expand-path)
                         ("lib/perl" "gui/nboss-customer-portal" "gui/customer-portal" "gui/partner-portal" "gui/sir"))
  (ffap-perl-module-path "lib/perl" "gui/nboss-customer-portal" "gui/customer-portal" "gui/partner-portal" "gui/sir"))))
 '(scroll-conservatively 0)
 '(scroll-preserve-screen-position t)
 '(scroll-step 0)
 '(server-mode t)
 '(server-use-tcp nil)
 '(shell-prompt-pattern "^[^$>
]*[#$%>] *\\([[0-9;]*[a-zA-Z] *\\)*")
 '(show-paren-style (quote parenthesis))
 '(slime-autodoc-use-multiline-p t)
 '(slime-complete-symbol*-fancy t)
 '(slime-complete-symbol-function (quote slime-complete-symbol*))
 '(starttls-gnutls-program "/usr/local/bin/gnutls-cli")
 '(tab-width 4)
 '(temp-buffer-max-height (lambda (buffer) (/ (- (frame-height) 2) 3)))
 '(temp-buffer-resize-mode nil)
 '(text-mode-hook (quote (text-mode-hook-identify pdc/turn-on-abbrev-mode)))
 '(tls-checktrust (quote ask))
 '(tls-program
(quote
 ("/usr/local/bin/gnutls-cli --insecure -p %p %h" "/usr/local/bin/gnutls-cli --insecure -p %p %h --protocols ssl3" "/usr/bin/openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator ":")
 '(vc-git-diff-switches "-b")
 '(vc-git-program "/u/virtual/pdc/bin/git")
 '(visible-bell nil)
 '(visual-scroll-margin nil)
 '(w3m-command "/opt/local/bin/w3m")
 '(w3m-use-cookies t)
 '(wdired-allow-to-change-permissions (quote advanced))
 '(whitespace-style (quote (tabs trailing space-before-tab)))
 '(woman-use-own-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aquamacs-variable-width ((t (:height 140 :family "Lucida Grande"))) t))
