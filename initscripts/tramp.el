(req-package tramp
  :init
  (progn
    (setq tramp-backup-directory-alist backup-directory-alist)
    (setq tramp-auto-save-directory "~/.tramp-autosaves"
          tramp-encoding-shell "/bin/bash"
          tramp-use-ssh-controlmaster-options nil
          tramp-remote-path '("/home/staff/pdc/bin"
                              tramp-default-remote-path
                              tramp-own-remote-path)
          tramp-sh-extra-args '(("/bash\\'" . "-norc -noprofile")
                                ("/zsh\\'"  . "-d -f"))
          tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$>\n]*#?[]#$%>] *\\([][][0-9;]*[a-zA-Z] *?\\)"))
  :config
  (add-to-list
 'tramp-methods
 '("scpnox"
  (tramp-login-program "ssh")
  (tramp-login-args
   (("-l" "%u")
    ("-p" "%p")
    ("-e" "none")
    ("%h")))
  (tramp-async-args
   (("-q")))
  (tramp-remote-shell "/bin/sh")
  (tramp-remote-shell-args
   ("-c"))
  (tramp-copy-program "scp")
  (tramp-copy-args
   (("-P" "%p")
    ("-p" "%k")
    ("-q")
    ("-r")))
  (tramp-copy-keep-date t)
  (tramp-copy-recursive t)
  (tramp-gw-args
   (("-o" "GlobalKnownHostsFile=/dev/null")
    ("-o" "UserKnownHostsFile=/dev/null")
    ("-o" "StrictHostKeyChecking=no")))
  (tramp-default-port 22))))


