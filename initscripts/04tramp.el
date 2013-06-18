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
  (tramp-default-port 22)))

(setq tramp-default-method "scpnox")
(setq tramp-backup-directory-alist backup-directory-alist)
