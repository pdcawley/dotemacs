;; 04tramp.el -- TRAMP customizations

;; Hopefully will (has?) become unnecessary
;; (defadvice cd
;;   (before remove-initial-colon-from-cd activate)
;;   "Remove an initial colon which is stopping tramp from working"
;;   (let ((path (ad-get-arg 0)))
;;     (when (char-equal (string-to-char path) ?:)
;;       (ad-set-arg 0 (substring path 1)))))

(setq tramp-methods
      (cons
       '("pdcssh"
         (tramp-login-program "ssh")
         (tramp-login-args
          (("-l" "%u")
           ("-p" "%p")
           ("-o" "ControlPath=%t.%%r@%%h:%%p")
           ("-o" "ControlMaster=yes")
           ("-e" "none")
           ("%h")))
         (tramp-async-args
          (("-q")))
         (tramp-remote-shell "/bin/sh")
         (tramp-remote-shell-args
          ("-c"))
         (tramp-gw-args
          (("-o" "GlobalKnownHostsFile=/dev/null")
           ("-o" "UserKnownHostsFile=/dev/null")
           ("-o" "StrictHostKeyChecking=no"))))
       tramp-methods))
