(require 'moz)
(require 'json)
(require 'thingatpt)
(require 'browse-url)

(eval-when-compile
   (require 'cl))

(defun dss/open-url ()
  "Open a new buffer containing the contents of URL.
http://github.com/technomancy/emacs-starter-kit/blob/master/starter-kit-defuns.el"
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mozilla integration
; FF settings in about:config
;   extensions.mozrepl.autoStart;true
;   extensions.mozrepl.initUrl;file://localhost/home/tavis/tr_toolchain/mozrepl/custom.js

(add-hook 'js2-mode-hook (lambda () (moz-minor-mode 1)))
;    * C-c C-s: open a MozRepl interaction buffer and switch to it
;    * C-c C-l: save the current buffer and load it in MozRepl
;    * C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;    * C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;    * C-c C-r: send the current region to MozRepl


;; (defun dss/moz-connect()
;;   (interactive)
;;   (global-set-key "\C-x\C-g"
;;                   (lambda ()
;;                     (interactive)
;;                     (save-buffer)
;;                     (dss/moz-eval-expression "this.BrowserReload()\n"))))
(add-to-list 'desktop-globals-to-save 'moz-repl-host)
(add-to-list 'desktop-globals-to-save 'moz-repl-port)

(setq moz-restart-repl-hooks '())
(defun dss/moz-restart-repl ()
  (interactive)
  (run-hooks 'moz-restart-repl-hooks)
  (if (and (buffer-live-p inferior-moz-buffer)
           (get-buffer-process inferior-moz-buffer))
      (progn
        (delete-process inferior-moz-buffer)
        ;; (kill-buffer inferior-moz-buffer)
        ))
  (sit-for 0.2)
  (set-process-query-on-exit-flag (inferior-moz-process) nil)
  (inferior-moz-process))

(defun dss/moz-reset ()
  (interactive)
  (progn
    (comint-send-string (inferior-moz-process)
                        (concat ;; moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                         moz-repl-name ".setenv('inputMode', 'syntax'); "
                         moz-repl-name ".setenv('printPrompt', true); undefined;"))
    (comint-send-string (inferior-moz-process)
                        (concat ;; moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                         moz-repl-name ".home(); "
                         ))))

(defun dss/moz-eval-buffer ()
  "Send full buffer of js to Moz."
  (interactive)
  (run-hooks 'dss/moz-pre-eval-hook)
  (if (equal major-mode 'coffee-mode)
      (dss/moz-coffee)
    (dss/moz-send-string (buffer-string)))
                                        ;(comint-send-string (inferior-moz-process) exp)
  )

(setq dss/moz-pre-eval-hook nil)

(defun dss/moz-enter-page ()
  (interactive)
  (dss/moz-send-string "repl.enter(content)")
  ;; (add-hook 'after-save-hook fname t t)
  )

(defun dss/moz-set-page-context ()
  (interactive)
  (add-hook 'dss/moz-pre-eval-hook 'dss/moz-enter-page t t))

(defun dss/moz-eval-expression (exp)
  "Send expression to Moz."
  (interactive "sJSEval: ")
                                        ;(comint-send-string (inferior-moz-process) exp)
  (dss/moz-send-string exp))

(defun trim-ws-in-string (string)
  "This function removes the leading and trailing whitespace from
   a string"
  (replace-regexp-in-string "\\(^[ \t]*\\|[ \t]*$\\)" "" string))

(defun dss/join-lines (s &optional sep)
  "remove blank lines from string and join what remains with sep"
  (let ((sep (or sep "")))
    (mapconcat #'identity (delq "" (split-string s "\n")) sep)))

(defun dss/moz-eval-expression-fresno (expr)
  (interactive "sExpr: ")
  (message (dss/local-shell-command-to-string
            (concat "~/src/fresno/fresno -j \""
                    expr
                    "\""))))

;;repl.home = function () { return this.enter(this._creationContext.content); }

(defun dss/moz-eval-para ()
  (interactive)
  (mark-paragraph)
  (setq mark-active nil)
  (call-interactively 'dss/flash-region)
  (call-interactively 'dss/moz-eval-region))

(defun dss/moz-eval-region-or-para ()
  (interactive)
  (if mark-active
      (call-interactively 'dss/moz-eval-region)
    (dss/moz-eval-para)))

(defun dss/moz-eval-region (start end)
  (interactive "r")
  (run-hooks 'dss/moz-pre-eval-hook)
  (if (equal major-mode 'coffee-mode)
      (progn
        ;; (call-interactively 'dss/coffee-send-region)
        (ignore-errors
          (dss/coffee-send-region start end))
        (sit-for 0.5)
        (message "sending to moz from coffee")
        (dss/moz-coffee))
    (let ((str (buffer-substring start end)))
      (message str)
      (dss/moz-eval-expression str))))

(defun dss/moz-send-string (str)
  (interactive "sJSEval: ")
  (let ((proc (inferior-moz-process)))
    (progn
      (comint-send-string proc
                          (concat moz-repl-name ".pushenv('printPrompt', 'inputMode'); "
                                  moz-repl-name ".setenv('printPrompt', false); "
                                  moz-repl-name ".setenv('inputMode', 'multiline'); "
                                  "undefined; \n"))
      ;; Give the previous line a chance to be evaluated on its own.  If
      ;; it gets concatenated to the following ones, we are doomed.
      (sleep-for 0.1)
      (comint-send-string proc str)
      (comint-send-string proc "\n--end-remote-input\n")
      (comint-send-string proc
                          (concat moz-repl-name ".popenv('inputMode', 'printPrompt'); "
                                  "undefined; \n"))
      (comint-send-string proc "\n--end-remote-input\n"))))

(defun dss/moz-eval-expression-capture (str)
  (interactive "sJSEval: ")
  (let* ((read-buffer "")
         (proc (inferior-moz-process))
         (comint-filt (process-filter proc)))
    (set-process-filter proc (lambda (proc string)
                               (setf read-buffer (concat read-buffer string))))
    (dss/moz-send-string str)
    (accept-process-output (inferior-moz-process) 0.75)
    ;; (while (not (string-match "^repl>" read-buffer)) (accept-process-output proc))
    (set-process-filter proc comint-filt)
                                        ;(message (mapconcat 'identity (butlast (split-string read-buffer "\n")) "\n"))
    (let ((result (mapconcat
                   'identity
                   (butlast (split-string read-buffer "\n")) "\n")))
      (substring result 0 (- (length result) 1)))))

(defun dss/moz-get-expression-value (expr &optional save-as-kill)
  (let ((value (dss/moz-eval-expression-capture
                (concat "repl.print(" expr ")"))))
    (if save-as-kill
        (kill-new value))
    value))

(defun dss/moz-repl-home ()
  (interactive)
  (dss/moz-eval-expression "repl.home()"))

(defun dss/moz-repl-content ()
  (interactive)
  (dss/moz-eval-expression "repl.enter(content)"))

(defun dss/moz-reload ()
  "Reload the url in the current tab"
  (interactive)
  ;; (dss/moz-repl-home)
  (dss/moz-eval-expression-fresno "repl._creationContext.BrowserReload()"))

(defun dss/moz-reload-delayed ()
  "Reload the url in the current tab"
  (interactive)
  (run-at-time "2 sec" nil 'dss/moz-reload))

;;; window.getBrowser().mTab[i]
(defun dss/moz-update (&rest ignored)
  "Update the remote mozrepl instance"
  (interactive)
  (comint-send-string (inferior-moz-process)
    (concat "content.document.body.innerHTML="
             (json-encode (buffer-string)) ";")))

(defun dss/moz-enable-auto-reload ()
  "Automatically reload the remote mozrepl when this buffer changes"
  (interactive)
  (add-hook 'after-save-hook 'dss/moz-reload-delayed t t))

(defun dss/moz-disable-auto-reload ()
  "Disable automatic mozrepl updates"
  (interactive)
  (remove-hook 'after-save-hook 'dss/moz-reload-delayed t))

;; (defun moz-find-next ()
;;   (interactive)
;;   (dss/moz-eval-expression "repl._creationContext.gBrowser.webBrowserFind.findNext()\n"))

;;;

(defun dss/moz-new-tab ()
  (interactive)
  (dss/moz-eval-expression-fresno "repl._creationContext.gBrowser.selectedTab = repl._creationContext.gBrowser.addTab()\n"))

(defun dss/moz-new-tab-url (url)
  (interactive "sURL:")
  (dss/moz-eval-expression
   (format "repl._creationContext.gBrowser.selectedTab = repl._creationContext.gBrowser.addTab('%s')\n" url)))

(defvar dss-browse-url-hook nil
  "Hook run on after each call of browser-url.")

(defun dss/browse-url-firefox-new-tab (url &optional new-window)
  "Open URL in a new tab in Mozilla."
  (interactive (browse-url-interactive-arg "URL: "))
  (dss/moz-new-tab-url url)
  (run-hooks 'dss-browse-url-hook))
(setq browse-url-browser-function 'dss/browse-url-firefox-new-tab)

(defun dss/moz-duplicate-tab ()
  (interactive)
  (dss/moz-eval-expression "repl._creationContext.gBrowser.duplicateTab(repl._creationContext.gBrowser.mCurrentTab)\n"))

(defun dss/moz-close-tab ()
  (interactive)
  (dss/moz-eval-expression-fresno "repl._creationContext.gBrowser.removeCurrentTab()"))

(defun dss/moz-back ()
  (interactive)
  (dss/moz-eval-expression-fresno "repl._creationContext.BrowserBack()"))

(defun dss/moz-forward ()
  (interactive)
  (dss/moz-eval-expression "repl._creationContext.BrowserForward()"))

(defun dss/moz-next-tab ()
  (interactive)
  (dss/moz-repl-home)
  (dss/moz-eval-expression "
var tabbrowser = repl._creationContext.window.getBrowser();
var _lastTab = tabbrowser.selectedTab;
tabbrowser.selectedTab = tabbrowser.mTabs[_lastTab._tPos+1];
"))

(defun dss/moz-print-tabs ()
  (interactive)
  (dss/moz-eval-expression-capture "
var tabbrowser = repl._creationContext.window.getBrowser();
for (var i=0, tab; tab = tabbrowser.mTabs[i]; i++) {
   repl.print('Tab '+i);
   repl.print(tab.linkedBrowser.contentDocument.title);
   repl.print(tab.linkedBrowser.contentDocument.documentURI);
   repl.print('---\\n');
}
"))

(defun dss/moz-get-url ()
  (interactive)
  (dss/moz-get-expression-value "content.location.href" t))

(defun dss/moz-get-html ()
  (interactive)
  (dss/moz-get-expression-value
   "content.document.documentElement.innerHTML" t))

(defun dss/moz-get-title ()
  (interactive)
  (dss/moz-get-expression-value
   "content.document.title" t))


(defun dss/moz-find-in-tab (text)
  (interactive "sWhat: ")
  (dss/moz-eval-expression
   (concat
    "repl._creationContext.gFindBar._findField.value = '" text "'
repl._creationContext.gFindBar.open()
repl._creationContext.gFindBar._find()")))

(defun dss/moz-find-again ()
  (interactive)
  (dss/moz-eval-expression "repl._creationContext.gFindBar._findAgain()"))

(defun dss/moz-previous-tab ()
  (interactive)
  (dss/moz-eval-expression "
var tabbrowser = repl._creationContext.window.getBrowser();
var _lastTab = tabbrowser.selectedTab;
tabbrowser.selectedTab = tabbrowser.mTabs[_lastTab._tPos-1];
"))

(defun dss/moz-select-tab (n)
  (interactive "nTab: ")
  (dss/moz-eval-expression
   (format "repl._creationContext.window.getBrowser().selectedTab = repl._creationContext.window.getBrowser().mTabs[%d]" (- n 1))))

(defun dss/moz-select-window (n)
  (interactive "nWindow: ")
  (dss/moz-eval-expression
   (format "repl.enter(repl.getWindows()[%d]);\n" (- n 1))))


;; (defvar emacspeak-moz-js-directory
;;   (expand-file-name "js" emacspeak-directory)
;;   "Directory where we keep js files.")

;; (defun emacspeak-moz-load-js-files (directory)
;;   "Load all .js files from specified directory."
;;   (declare (special moz-repl-name))
;;   (comint-send-string
;;    (inferior-moz-process)
;;    (mapconcat
;;     #'(lambda (file)
;;         (format "%s.load('file://localhost%s')"
;;                 moz-repl-name file))
;;     (directory-files directory  'full "js$")
;;     ";")))

;; (defun emacspeak-moz-init ()
;;   "Load emacspeak.js file, and initialize context."
;;   (declare (special moz-repl-name
;;                     emacspeak-directory emacspeak-moz-js-directory))
;;   (comint-send-string
;;    (inferior-moz-process)
;;    (format
;;     "%s.load('file://localhost%s') \;
;; %s.emacspeak = new Emacspeak('%s')\;
;; %s.emacspeak.init()\;"
;;     moz-repl-name (expand-file-name "emacspeak.js" emacspeak-moz-js-directory)
;;     moz-repl-name emacspeak-directory
;;     moz-repl-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; function getWindows() {
;;     var windowEnum = Cc['@mozilla.org/appshell/window-mediator;1']
;;         .getService(Ci.nsIWindowMediator).getEnumerator('');
;;     var windows = [];
;;     while(windowEnum.hasMoreElements())
;;         windows.push(windowEnum.getNext());

;;     return windows;
;; }

;;; http://wiki.github.com/bard/mozrepl/command-simple-dom-grabber
;; function grab() {
;;     var document = this._workContext.document;
;;     if(!document)
;;         throw new Error('No document around.');
;;     var result = {};
;;     var grabber = function(event) {
;;         result.event = event;
;;         event.stopPropagation();
;;         document.removeEventListener('click', grabber, true);
;;     };
;;     document.addEventListener('click', grabber, true);
;;     return result;
;; }



; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/OpenNewTab.pm
; function (url, selected) {
;   selected = (selected) ? true : false;
;   var tab;
;   try {
;     tab = window.top.getBrowser ().addTab (url);
;
;     if (!tab) {
;       return false;
;     }
;
;     if (selected) {
;       window.top.getBrowser ().selectedTab = tab;
;     }
;   }
;   catch (e) {
;     return false;
;   }
;   return true;
; }

; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/Location.pm
; function (all) {
;   function getLocationAsJSON (tWindow) {
;     var json = {};
;     for (var p in tWindow.location) {
;       var type;
;
;       try {
;         type = typeof tWindow.location [p];
;       }
;       catch (e) {
;         continue;
;       }
;
;       if (type == "object" || type == "function" || type == "undefined") {
;         continue;
;       }
;       json [p] = tWindow.location [p];
;     }
;     return json;
;   }
;
;   if (all) {
;       return JSONstring.make (Array.prototype.map.call (
;           window.getBrowser ().tabContainer.childNodes,
;           function (tab) {
;               return JSONString.make (getLocationAsJSON (tab.linkedBrowser.contentWindow));
;           }));
;   }
;   else {
;       return JSONstring.make (getLocationAsJSON (window.getBrowser ().contentWindow));
;   }
; }

; http://cpansearch.perl.org/src/ZIGOROU/MozRepl-Plugin-LinkTools-0.01/lib/MozRepl/Plugin/PageReload.pm
; (function (args) {
;   try {
;     if (args.regex && args.regex instanceof RegExp) {
;       var rcnt = 0;
;
;       Array.prototype.forEach.call (
;                                    window.getBrowser ().tabContainer.childNodes,
;                                    function (tab) {
;                                      var tLocation = tab.linkedBrowser.contentWindow.location;
;
;                                      if (args.regex.test (tLocation.href)) {
;                                        tLocation.reload ();
;                                        rcnt++;
;                                      }
;                                    });
;
;       return rcnt;
;     }
;     else if (typeof args.tab_index == "number") {
;       window.getBrowser ().getBrowserAtIndex (tab_index).linkedBrowser.contentWindow.location.reload ();
;       return 1;
;     }
;     else {
;       window.getBrowser ().contentWindow.location.reload ();
;       return 1;
;     }
;   }
;   catch (e) {
;     return 0;
;   }
; }) ({ tab_index: [% tab_index %], regex: [% regex %] });
;

;(require 'webjump)
;(global-set-key [f2] 'webjump)
;(setq webjump-sites
;      (append '(
;  ("Google Image Search" .
;   [simple-query "images.google.com" "images.google.com/images?hl=en&q=" ""])
;  ("Flickr Search" .
;   [simple-query "www.flickr.com" "flickr.com/search/?q=" ""])
;  )
;       webjump-sample-sites))
;

;; http://www.rlazo.org/blog/entry/2008/sep/13/do-you-use-google-to-find-definitions/


(require 'mm-url)
(defun dss/google-define-word-or-phrase (query)
  (interactive "sInsert word or phrase to search: ")
  (let* ((url (concat "http://www.google.com.pe/search?hl=en&q=define%3A"
              (replace-regexp-in-string " " "+" query)))
     (definition
       (save-excursion
         (with-temp-buffer
           (mm-url-insert url)
           (goto-char (point-min))
           (if (search-forward "No definitions found of " nil t)
           "No definitions found"
         (buffer-substring (search-forward "<li>") (- (search-forward "<") 1)))))))
    (message "%s: %s" query definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/emacs-w3m")
;; (setq w3m-icon-directory "/usr/share/emacs/etc/emacs-w3m")
;; (require 'w3m-load)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'dss-browser-integration)
