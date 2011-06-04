;; Use cperl mode instead of the default perl mode

(add-to-list 'load-path (concat dotfiles-dir "/cperl-mode"))

(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t" . perl-mode))

(defun pdc:path->perl-module (path)
  (if (string-match "/lib/\\(.*\\)\\.pm" path)
      (let ((module (match-string 1 path)))
        (while (string-match "/" module)
          (setq module (replace-match "::" nil nil module)))
        module)
    nil))

;; Load an application template in a new unattached buffer...

(defun application-template-pl (  )
  "Inserts the standard Perl application template"  
                                        ; For help and info.

  (interactive "*")                                
                                        ; Make this user accessible.

  (switch-to-buffer "application-template-pl")
  (insert-file "~/.code_templates/perl_application.pl"))

;; Load a module template in a new unattached buffer...

(defun module-template-pm (  )
  "Inserts the standard Perl module template"       
  (interactive "*")                                 
  (switch-to-buffer "module-template-pm")
  (insert-file "~/.code_templates/perl_module.pm"))
  
