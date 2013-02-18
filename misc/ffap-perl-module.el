;;; ffap-perl-module.el --- find perl module at point with ffap

;; Copyright 2009, 2010, 2011, 2012 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 19
;; Keywords: files, ffap, perl
;; URL: http://user42.tuxfamily.org/ffap-perl-module/index.html
;; EmacsWiki: FindFileAtPoint

;; ffap-perl-module.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ffap-perl-module.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This spot of code lets M-x ffap find the source file for a Perl module.
;; For example Foo::Bar becomes /usr/share/perl5/Foo/Bar.pm or wherever in
;; the path.
;;
;; Variable names or sub-packages are stripped, and a prefix is added if
;; unique.  See the `ffap-perl-module-file-at-point' docstring below for
;; details.
;;
;; The lookup is independent of the major mode, so you can be in Man-mode,
;; diff-mode, pod-mode or whatever and still go to Perl source.

;;; Emacsen:

;; Designed for Emacs 21 up.  Works in Emacs 20 and XEmacs 21 except doesn't
;; recognise non-ASCII in Perl class names and variable names.

;;; Install:

;; Put ffap-perl-module.el in one of your `load-path' directories and the
;; following in your .emacs
;;
;;     (eval-after-load "ffap" '(require 'ffap-perl-module))
;;

;;; History:
;; 
;; Version 1 - the first version
;; Version 2 - allow point at start of "use", "no" or "require" statement
;;           - prefer "Changes" as file, not DBI/Changes.pm
;; Version 3 - use [:alpha:] etc to match non-ascii variable names
;; Version 4 - cope with non-existent `default-directory'
;; Version 5 - fix for unqualified variables $FOO
;; Version 6 - drop the enabling cookie as it can be too intrusive by default
;; Version 7 - use pipe rather than pty for subprocess
;; Version 8 - prefer file at point over search for perl module
;; Version 9 - set ffap-string-at-point variable
;; Version 10 - undo defadvice on unload-feature
;; Version 11 - don't search for RFCs, notice $HOME etc vars
;; Version 12 - express dependency on 'advice
;; Version 13 - ignore errors from substitute-in-filename
;; Version 14 - recognise -MFoo::Bar and -MO=Foo perl command line
;; Version 15 - recognise Moose/Mouse extends 'Some::Class'
;; Version 16 - fix operators "&" and "&&" are not funcs
;;            - try one-level suffix prune before big prefix expand search
;; Version 17 - recognise Symbol.pm etc one word with .pm
;; Version 18 - allow leading numbers in sub-parts, like Encode::KR::2022_KR
;; Version 19 - narrow for speedup in big buffers

;;; Code:

;; for `ad-find-advice' macro when running uncompiled,
;; don't unload 'advice before ffap-perl-module-unload-function
(require 'advice)

;;;###autoload
(defcustom ffap-perl-module-path nil
  "List of directories to search for Perl modules.
If nil then function `ffap-perl-module-path' initializes it from
Perl's @INC when you first attempt an `ffap' Perl module lookup."
  :type  '(repeat directory)
  :group 'ffap
  :link '(url-link
          :tag "ffap-perl-module.el home page"
          "http://user42.tuxfamily.org/ffap-perl-module/index.html"))

(defadvice ffap-string-at-point (around ffap-perl-module activate)
  "Extract a Perl module filename at point.
See `ffap-perl-module-file-at-point' for details."

  ;; The expand-prefix stuff is a bit slow, so only run for mode==nil, not
  ;; url, machine, etc.  For the same reason prefer a filename at point over
  ;; a search, in particular this stops "Makefile" at point from churning
  ;; all the perl dirs when it's a filename rather than a module.
  ;;
  ;; args: (ffap-string-at-point &optional MODE)
  (unless (let ((mode (ad-get-arg 0)))
            (and (not mode)
                 (not (ffap-perl-module-existing-file-at-point-p))
                 (let ((filename (ffap-perl-module-file-at-point)))
                   (and filename
                        (progn
                          (set-text-properties 0 (length filename) nil filename)
                          (setq ad-return-value
                                (setq ffap-string-at-point filename)))))))
    ad-do-it))

(defun ffap-perl-module-unload-function ()
  "Remove defadvice from function `ffap-string-at-point'.
This is called by `unload-feature'."
  (when (ad-find-advice 'ffap-string-at-point 'around 'ffap-perl-module)
    (ad-remove-advice   'ffap-string-at-point 'around 'ffap-perl-module)
    (ad-activate        'ffap-string-at-point))
  nil) ;; and do normal unload-feature actions too

(defun ffap-perl-module-existing-file-at-point-p ()
  "Return non-nil if there's a filename at point and that file exists."

  ;; `substitute-in-file-name' throws an error for an unknown environment
  ;; variable "$NOSUCH" or "${NOSUCH}", or an unpaired brace "${HOME",
  ;; though in emacs22 up it's more relaxed, allowing "$NOSUCH" but error on
  ;; "${NOSUCH}".  Braces are not in (ffap-string-at-point 'file) by
  ;; default, only if you extend the chars in
  ;; `ffap-string-at-point-mode-alist'.
  ;;
  (let ((filename (ffap-string-at-point 'file)))
    (and filename
         ;; unknown ${NOSUCH} variables treated as a non-existent filename
         (condition-case nil
             (setq filename (substitute-in-file-name filename))
           (error nil))
         ;; any remote syntax filename is considered to exist
         (or (ffap-file-remote-p filename)
             (file-exists-p filename)))))


(defun ffap-perl-module-path ()
  "Return a list of directory names to search for Perl modules.
This function returns variable `ffap-perl-module-path' if it's not nil,
or initializes that by running \"perl -e print @INC\" for the
places Perl will look, which is usually various /usr/share, /usr/local,
and whatever your PERL5LIB says.

The current directory \".\" which is normally in @INC is
deliberately excluded from the default calculation.  It's a bit
of a security hole and too easily makes `ffap-perl-module-expand-prefix'
churn deep through irrelevant directories."

  (or ffap-perl-module-path
      (with-temp-buffer
        (let ((default-directory "/") ;; in case inherit a non-existent
              (coding-system-for-read file-name-coding-system)
              (process-connection-type nil)) ;; pipe
          (call-process "perl" nil t nil "-e" "$,='\n'; print @INC"))
        (setq ffap-perl-module-path
              (or (delete "." (split-string (buffer-string) "\n"))
                  ;; something non-empty as a fallback
                  '("/usr/share/perl"))))))

;; No [:alnum:] etc in emacs20,xemacs21, fallback to A-Z etc.  Which means
;; unicode in variable names doesn't match there, you have to have point on
;; the (ascii) package name part.  What would be an easy better way?
;;
(eval-and-compile ;; used in compile-time concats
  (defconst ffap-perl-module-word-regexp
    (eval-when-compile
      (let* ((alpha (if (string-match "[[:alpha:]]" "A")
                        "[:alpha:]" "A-Za-z0-9"))
             (alnum (if (string-match "[[:alnum:]]" "A")
                        "[:alnum:]" "A-Za-z0-9")))
        (concat "[" alpha "_][" alnum "_]*")))
    "Regexp for a module name without any \"::\".
This matches for instance \"FindBin\".  It doesn't match
\"Foo::Bar\", or only the \"Foo\" part.

\[:alpha:] and [:alnum:] are used when available to allow unicode
in the package name (if you trust that to match up with the
filename on disk).  A-Z fallbacks are used for xemacs21."))

(defconst ffap-perl-module-directory-regexp
  (eval-when-compile
    (let* ((alpha (if (string-match "[[:alpha:]]" "A")
                      "[:alpha:]" "A-Za-z0-9"))
           (alnum (if (string-match "[[:alnum:]]" "A")
                      "[:alnum:]" "A-Za-z0-9"))
           (later-word (concat "[" alnum "_]*")))
      (concat "\\`" later-word "\\'")))
  "Regexp for a directory name for packages.
This matches only a single word like \"Moose\" or \"2022_KR\"
without any \"/\"s etc.  It doesn't match a .pm extension, so as
to save stat()ing them, and doesn't match . or .. to avoid an
infinite loop searching!

\[:alpha:] and [:alnum:] are used when available to maybe allow
unicode in package names, if you trust that to match up with the
filename on disk.  A-Z fallbacks are used for xemacs21.")

(eval-and-compile ;; used in compile-time concats
  (defconst ffap-perl-module-qualif-regexp
    (eval-when-compile
      (let* ((alpha (if (string-match "[[:alpha:]]" "A")
                        "[:alpha:]" "A-Za-z0-9"))
             (alnum (if (string-match "[[:alnum:]]" "A")
                        "[:alnum:]" "A-Za-z0-9"))
             (first-word  (concat "[" alpha "_][" alnum "_]*"))
             (later-word  (concat "[" alnum "_]*")))
        (concat "\\(" first-word "\\(" "::" later-word "\\)*\\)")))
    "Regexp for a name with optional :: qualifiers.
This matches for instance \"FindBin\" or \"Moose::Util::something\"
or \"Timebase::10Min\".

\[:alpha:] and [:alnum:] are used when available to allow unicode
in variable names, and even in the package names (if you trust
that to match up with the filename on disk).  A-Z fallbacks are
used for xemacs21."))

(defun ffap-perl-module-file-at-point ()
  "Find the filename for a Perl module at point.
For example with point on Foo::Bar the return could be
\"/usr/share/perl5/Foo/Bar.pm\".  If there's nothing in
`ffap-perl-module-path' for a package at point then the return is
nil.

* -MFoo::Bar is recognised as a command line module Foo::Bar, and
  -MO=Concise as compile output B::Concise etc.

* $Foo::Bar::QUUX, &{Foo::Bar::QUUX} etc are variable or
  subroutine names and the package part is Foo::Bar.  Currently
  this isn't applied to a plain calls Foo::Bar::func(), but
  they're pruned by the next rule so normally work ok.

* Foo::Bar::Quux is pruned back to Foo::Bar, or just Foo, if the
  full package doesn't exist.  This is good if a single file
  defines a set of sub-packages, or if Quux is actually a
  constant subr, etc.  It hopefully gets you close to the right
  package at least.

* \"use Foo\", \"no Foo\", \"require Foo\" and Moose style
  \"extends 'Foo'\" all work with point in the \"use\" part etc
  as well as the package name part, which is good when point is
  at the start of such a line.

* Client::DNS or similar shorthand is expanded to say
  POE::Component::Client::DNS if that's the only Client::DNS.
  This is good in documentation where a long package prefix might
  be omitted, eg. POE or Perl::Critic.

  The search for this may take several seconds depending how much
  is in your `ffap-perl-module-path' and subdirectories.  Use
  \\[keyboard-quit] in the usual way to stop it.

* constant.pm etc one word with .pm or .pod is recognised, it
  being sometimes clearer to write a .pm when referring to the
  toplevel modules.

* constant etc one word without any \"::\" will go to constant.pm
  for the toplevel Perl modules.  But a leading or trailing / or
  . is taken to mean a filename, not a package name, and the
  return is nil in that case.  The latter rule prevents for
  instance \"sort.el\" from offering sort.pm.

* If there's no .pm file for the package but there's a .pod then
  that's returned.  This is good for pseudo-packages like
  Module::Build::Cookbook which are just documentation.  \(A
  toplevel like Carp.pod with \".pod\" of course prefers the .pod
  to the .pm.)

* PoCo is recognised as an abbreviation for POE::Component, as
  found in documentation (the code is always the full name).

* Non-ascii variable names work in Emacs, but are not matched in
  XEmacs21.  Put point on the package name part instead.

  Non-ascii package names are matched in Emacs, but it's up to
  you to ensure any Perl \"use utf8\" and your locale and Emacs
  `file-name-coding-system' and the actual bytes in the name on
  disk all coincide.  That may be asking for trouble most of the
  time! :-)

This function is designed for use under `ffap' so it sets
`ffap-string-at-point-region' to the part of the buffer
identified as the package name.

`ffap' normally takes \"constant.pm\" etc as a host name, since
.pm is a toplevel internet domain (\"Saint Pierre and
Miquelon\").  But with this ffap-perl-module, the way `ffap'
looks for local files before machines means a .pm Perl module is
tried before a ping of a foo.pm machine.

The ffap-perl-module.el home page is
URL `http://user42.tuxfamily.org/ffap-perl-module/index.html'"

  (eval-and-compile ;; quieten the byte compiler too
    (require 'ffap)
    (require 'thingatpt))

  (save-excursion
    ;; If point is just after a word then go back one char to let
    ;; thing-at-point-looking-at match on that previous word.  Is there a
    ;; more general way to get this effect?
    (and (not (bobp))
         (save-excursion
           (goto-char (1- (point)))
           (looking-at "\\S-\\(\\s-\\|\\'\\)"))
         (goto-char (1- (point))))

    ;; Narrow to the current line as a speedup for big buffers.  This limits
    ;; the amount of searching back and forward `thing-at-point-looking-at'
    ;; will do in its works-around for the way re-search-backward doesn't
    ;; match across point.
    ;;
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))

      ;; IN-USE-P is non-nil if we're looking at a "use", "no" or "require"
      (let* ((case-fold-search nil) ;; case sensitive "use" etc
             (ext1 '(".pm"))
             (ext2 '(".pod"))
             type)

        ;; `thing-at-point-looking-at' doesn't work well on a pattern with
        ;; optional variable length prefix like say "\\(@\\s-*\\)?foo".  It's
        ;; fine when you're on the @ of "@ foo", but if you're on the foo then
        ;; the back-up char-by-char fails to match the space in between and
        ;; stops there, without seeing there's an earlier @ which would
        ;; succeed.  The way `re-search-backward' won't match across its start
        ;; point is the basic culprit; the gambits in
        ;; `thing-at-point-looking-at' to get around that don't cope with a
        ;; multiple-char optional prefix.
        ;;
        ;; So the strategy is to try a variable name, variable name in {}, a
        ;; use/no/require, then a bare word.  The backslashes "\" in the
        ;; variables mean you can have point on a ref like \&foo and get foo
        ;; matched.  There's nothing else done with the fact it's making a
        ;; ref, it's just for point at the start of such a form.
        ;;
        (and (or
              ;; command line: -MList::Util
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\(?:\\(?:\\`\\|\\s-\\)-MO=\\)"
                              ffap-perl-module-qualif-regexp)))
                   (setq type 'use-B))

              ;; command line: -MO=Concise
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\(?:\\(?:\\`\\|\\s-\\)-M\\)"
                              ffap-perl-module-qualif-regexp)))
                   (setq type 'use))

              ;; variable: $List::Util::something
              ;;           @Foo::Bar::something
              ;; varref:   \\\%List::Util::something
              ;; subr:     &List::Util::first
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\\\*[$@%&]"
                              ffap-perl-module-qualif-regexp)))
                   (setq type 'variable))

              ;; variable: ${Foo::Bar::something}
              ;; varref:   \\@{Foo::Bar::something}
              ;; etc
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\\\*[$@%&]\\s-*{\\s-*"
                              ffap-perl-module-qualif-regexp
                              "\\s-*}")))
                   (setq type 'variable))

              ;; module: use warnings ...
              ;;         no strict;
              ;;         require List::Util;
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\(?:use\\|no\\|require\\)\\s-+"
                              ffap-perl-module-qualif-regexp)))
                   (setq type 'use))

              ;; Moose: extends 'List::Util'
              ;;        extends "List::Util"
              ;;        extends qw(Pod::Elemental)
              ;; same in Mouse
              (and (thing-at-point-looking-at
                    (eval-when-compile
                      (concat "\\(?:extends\\)\\s-+\\(?:['\"]\\|qw(\\)"
                              ffap-perl-module-qualif-regexp
                              "['\")]")))
                   (setq type 'use))

              ;; one word with pm: constant.pm Test.pod Inline.pm Inline.pod
              (and (thing-at-point-looking-at
                    (eval-when-compile ;; match 1 part to highlight and search
                      (concat "\\(" ffap-perl-module-word-regexp
                              "\\)\\.\\(pm\\|pod\\)")))
                   (progn
                     (when (equal (match-string 2) "pod")
                       (setq ext1 '(".pod")  ;; search pod first if .pod
                             ext2 '(".pm")))
                     (setq type 'pm)))

              ;; plain: List::Util
              ;; double-colon bareword: List::Util::
              ;; (match the final :: so it works with point on those colons)
              (thing-at-point-looking-at
               (eval-when-compile
                 (concat ffap-perl-module-qualif-regexp "\\(::\\)?"))))

             ;; don't chase down a bare word "Changes", prefer a normal ffap
             ;; of a file called Changes in the local directory instead of
             ;; DBI/Changes.pm which is the DBI package news file (it's a bit
             ;; nasty hard coding an exception like this, but it gets the
             ;; right effect ... maybe some other common words shouldn't be
             ;; chased too)
             ;; `type' "use Changes" or "$Changes" or "Changes.pm" ok to continue
             (or type
                 (not (equal (match-string 1) "Changes")))

             ;; don't chase down an RFC, prefer normal ffap lookup of that
             ;; `type' "use RFC" or "$RFC" or "RFC.pm" ok to continue
             (or type
                 (not (save-match-data
                        (string-match "\\`RFC[ 0-9]*\\'" (match-string 1)))))

             ;; leading or trailing / or . on a single word means a filename
             ;; `type' "use Foo." or "$Foo." or "Foo.pm." ok to continue
             (or type
                 (match-beginning 2) ;; match 2 means multi-word, is ok
                 (and (not (memq (char-before (match-beginning 0)) '(?/ ?.)))
                      (not (memq (char-after  (match-end 0))       '(?/ ?.)))))

             ;; functions and variables $FOO or &foo must have at least one ::
             ;; qualifier for the package part
             (or (not (eq type 'variable))
                 (match-beginning 2))

             ;; for variables etc beginning $ @ % & strip the variable name to
             ;; get the package part
             (setq ffap-string-at-point-region
                   (list (match-beginning 1)
                         (if (eq type 'variable)
                             (match-beginning 2) ;; before last "::foo"
                           (match-end 1))))

             (let ((modname (apply 'buffer-substring
                                   ffap-string-at-point-region)))
               (if (eq type 'use-B)
                   (setq modname (concat "B::" modname)
                         type    'use))
               (let ((basename (ffap-perl-module-modname-to-filename modname)))

                 (when (string-match "\\`PoCo\\(::.*\\)?\\'" modname)
                   (setq modname (concat "POE::Component"
                                         (match-string 1 modname))))

                 ;; prefer .pm over .pod, even if .pod is earlier in the path
                 (or (ffap-locate-file basename ext1 (ffap-perl-module-path))
                     (ffap-locate-file basename ext2 (ffap-perl-module-path))

                     ;; try to prune one level, to avoid the big expand search
                     ;; for a List::Util::first etc call
                     (ffap-perl-module-prune-suffix modname 1)

                     ;; if there's no exact match then try the prefix business
                     ;; (but not on variables), then suffix pruning
                     (and (not (eq type 'variable))
                          (ffap-perl-module-expand-prefix modname))

                     (ffap-perl-module-prune-suffix modname)))))))))

(defun ffap-perl-module-expand-prefix (modname)
  "Try to find MODNAME by putting a package prefix on it.
This some internals of `ffap-perl-module-file-at-point'.

MODNAME like \"Foo::Bar\" is sought with some prefix on it, like
\"Xyzzy::Foo::Bar\".  This is done by traversing all directories
and subdirectories of `ffap-perl-module-path', which might take a
few seconds if you've got a lot of stuff.

If there's a single such expanded package name then the filename
is returned, if not nil is returned.  If there's no .pm files at
all for MODNAME, then .pod is sought instead with the same
rules."

  (eval-and-compile (require 'cl))
  (catch 'stop
    (let* ((basename     (ffap-perl-module-modname-to-filename modname))
           (pm-basename  (concat basename ".pm"))
           (pod-basename (concat basename ".pm"))
           default-directory
           found-pod-pkg found-pod-filename
           found-pm-pkg  found-pm-filename)
      (dolist (pathdir (ffap-perl-module-path))
        ;; DIRLIST is absolute paths of directories to contemplate.  An
        ;; entry is taken off to inspect and its subdirectories are pushed
        ;; on, until no further directories and subdirectories exist.
        (let ((dirlist (condition-case nil
                           (directory-files
                            pathdir t ffap-perl-module-directory-regexp
                            t)  ;; no sort
                         (error nil))))
          ;; toplevel "auto" only has AutoSplit .al files
          ;; toplevel "LocaleData" only has .mo files for Locale::TextDomain
          ;; exclude these to shorten the search
          (setq dirlist (remove* "/\\(auto\\|LocaleData\\)\\'"
                                 dirlist :test 'string-match))
          (while dirlist
            ;; A few attempts (in emacs22) had it faster to set
            ;; default-directory and let directory-files give absolute
            ;; filenames, rather than concat directory and basename in lisp.
            ;; Ditto faster to stay iterative style with a DIRLIST than
            ;; recursive function calls to traverse.
            ;;
            ;; `file-name-as-directory' to ensure trailing slash,
            ;; directory-files gives the name only.
            ;;
            (setq default-directory (file-name-as-directory (car dirlist)))
            (setq dirlist (cdr dirlist))

            ;; diagnostic for what's traversed ...
            ;; (let ((dir default-directory))
            ;;   (with-current-buffer (get-buffer-create "x")
            ;;     (insert dir "\n")))

            (when (file-exists-p pm-basename)
              ;; pkg is without lib path part, eg. "/POE/Component/Client/",
              ;; just used to check uniqueness, ignoring shadowed copies
              ;; under different `pathdir's
              (let ((pkg (substring default-directory (length pathdir))))
                (if found-pm-pkg
                    (unless (equal pkg found-pm-pkg)
                      (throw 'stop nil)) ;; not unique, no good
                  (setq found-pm-pkg pkg)
                  (setq found-pm-filename
                        (concat default-directory pm-basename)))))

            (when (file-exists-p pod-basename)
              (let ((pkg (substring default-directory (length pathdir))))
                (if found-pod-pkg
                    (setq found-pod-filename nil) ;; not unique, discard
                  (setq found-pod-pkg pkg)
                  (setq found-pod-filename
                        (concat default-directory pod-basename)))))

            ;; Appending subdirs to dirlist means breadth-first traversal.
            ;; That might have a slightly better chance of seeing a
            ;; duplicate package name for some generic kind of word.  But a
            ;; successful search must traverse everything, so the order
            ;; doesn't matter all that much.
            (setq dirlist (nconc dirlist
                                 (condition-case nil
                                     (directory-files
                                      default-directory t
                                      ffap-perl-module-directory-regexp
                                      t)  ;; no sort
                                   (error nil)))))))
      (or found-pm-filename
          found-pod-filename))))

(defun ffap-perl-module-prune-suffix (modname &optional limit)
  ;; checkdoc-params: (modname limit)
  "Try to match MODNAME with suffix parts pruned off.
This is an internal part of `ffap-perl-module-file-at-point.

MODNAME like \"Aaa::Bbb::Ccc::Ddd\" is looked up shortened first
to \"Aaa::Bbb::Ccc\" then \"Aaa::Bbb\" and finally \"Aaa\".  The
return is a filename string, or nil if not found.  At each
pruning level .pm is tried then .pod.  If LIMIT is not nil then
it's the number of prune levels to try.

If found then the endpoint in `ffap-string-at-point-region' is
shortened according to how much was pruned off MODNAME."

  (let ((orig modname)
        (path (ffap-perl-module-path)))
    (catch 'stop
      (while modname
        (let* ((basename (ffap-perl-module-modname-to-filename modname))
               ;; prefer .pm over .pod, even if .pod is earlier in the path
               (filename (or (ffap-locate-file basename '(".pm") path)
                             (ffap-locate-file basename '(".pod") path))))

          (when filename
            ;; found, adjust region for how much trimmed to make MODNAME;
            ;; `max' not to be before the start point in case something
            ;; freaky has happened pruning an expanded abbreviation back
            ;; beyond the expansion ...
            (setq ffap-string-at-point-region
                  (list (car ffap-string-at-point-region)
                        (max (car ffap-string-at-point-region)
                             (- (cadr ffap-string-at-point-region)
                                (- (length orig) (length modname))))))
            (throw 'stop filename))

          (and limit
               (< (decf limit) 0)
               (throw 'stop nil))
          
          ;; strip last so Foo::Bar::Quux becomes Foo::Bar, or nil when no
          ;; more "::"s
          (setq modname (and (string-match "\\(.*\\)::" modname)
                             (match-string 1 modname))))))))

(defun ffap-perl-module-modname-to-filename (modname)
  "Return a filename for Perl module MODNAME.
MODNAME is a string like \"Foo::Bar::Quux\", the return simply
has each \"::\" turned into \"/\" like \"Foo/Bar/Quux\"."
  (mapconcat 'identity (split-string modname ":+") "/"))

;; LocalWords: usr docstring initializes func MFoo Quux subr PoCo Xyzzy Aaa Bbb Ccc Ddd stat alnum fallbacks perl filename filenames unicode ok subdirectories toplevel ascii utf internet Miquelon eg ing el

(provide 'ffap-perl-module)

;;; ffap-perl-module.el ends here
