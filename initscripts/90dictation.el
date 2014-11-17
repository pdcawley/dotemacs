(require 'dss-codenav-helpers)
(require 'dss-tmp-files)



;; (defun pdc/paredit-backward-delete ()
;;   (interactive)
;;   (if mark-active
;;       (call-interactively 'paredit-delete-region)
;;     (paredit-backward-delete)))
;; (bind-key "DEL" 'pdc/paredit-backward-delete paredit-mode-map)

;; (defcustom udp-buffer "*from-dragon*"
;;   "buffer messages from Dragon"
;;   :type 'integer
;;   :group 'dictation)



;; (defun pdc/udp-filter (process message)
;;   (with-current-buffer (process-buffer process)
;;     (goto-char (point-max))
;;     (if (string-prefix-p "(" message)
;;         (eval (read message))
;;       (insert message))))

;; (defvar dss-emacs-port 19999)

;; (defvar dss-udp-server
;;   (make-network-process
;;    :name "dss-udp-server"
;;    :server t
;;    :local "0.0.0.0"
;;    :type 'datagram
;;    :service dss-emacs-port
;;    :filter 'pdc/udp-filter
;;    :noquery t
;;    :buffer udp-buffer
;;    ))

;; (defvar dss-dragon-hostname "10.0.1.17")
;; (defvar dss-dragon-port 19999)

;; (defun dss/dragon-udp-client-process ()
;;   (let ((proc (get-process "dss-dragon-client")))
;;     (if (not proc)
;;         (progn (setq proc
;;                      (make-network-process
;;                       :name "dss-dragon-client"
;;                       :coding 'binary
;;                       :host dss-dragon-hostname
;;                       :service dss-dragon-port
;;                       :type 'datagram)))
;;       (set-process-query-on-exit-flag proc nil))
;;     proc))

;; (defun dss/dragon-kill-client-process ()
;;   (interactive)
;;   (delete-process (get-process "dss-dragon-client")))

;; (defun dss/dragon-send-string (str)
;;     (interactive "sWhat:")
;;     (process-send-string (dss/dragon-udp-client-process) str))

;; (defun dss/dragon-run (str)
;;   (interactive "sEval: ")
;;   (let* ((read-buffer "")
;;          (proc dss-udp-server)
;;          comint-filt (process-filter proc))
;;     (set-process-filter proc
;;                         (lambda (proc string)
;;                           (setf read-buffer (concat read-buffer string))))
;;     (dss/dragon-send-string str)
;;     (accept-process-output proc 0.2)
;;     (set-process-filter proc comint-filt)
;;     read-buffer))

;; (defun dss/dragon-eval (str)
;;   (interactive "sEval: ")
;;   (dss/dragon-run
;;    (concat
;;     "run_emacsclient(" str ")")))

;; (defun dss/dragon-is-mic-on? ()
;;   (interactive)
;;   (string-equal "on" (dss/dragon-eval "natlink.getMicState")))

;; (defun dss/dragon-toggle-mic ()
;;   (interactive)
;;   (dss/dragon-unlock)
;;   (dss/dragon-send-string
;;    (concat
;;     "natlink.setMicState('on') "
;;     "if natlink.getMicState() in ('off', 'sleeping') "
;;     "else natlink.setMicState('sleeping')")))


;; (defun dss/dragon-off ()
;;   (interactive)
;;   (dss/dragon-send-string "natlink.setMicState('off'"))


;; (defun dss/dragon-on ()
;;   (interactive)
;;   (dss/dragon-unlock)
;;   (dss/dragon-send-string "natlink.setMicState('on')"))

;; (defun dss/dragon-sleep ()
;;   (interactive)
;;   (dss/dragon-send-string "natlink.setMicState('sleeping')"))

;; (setq dss-dragon-status-modeline-colours
;;       (list
;;        ;;
;;        '("on" . "#005f00")
;;        '("sleeping" . "#af5f00")
;;        '("locked" . "#5f0000")
;;        '("off" . "darkred")
;;        (cons "" "#0000ff")))

;; (defun dss/dragon-sync-modeline (&optional state)
;;   (interactive)
;;   (let ((state (or state (dss/dragon-eval "natlink.getMicState()"))))
;;     (set-face-background
;;      'modeline
;;      (cdr (assoc-string state dss-dragon-status-modeline-colors)))))

;; (setq *dss-dragon-locked* nil)
;; (setq *dss-dragon-state* nil)

;; (defun dss/dragon-lock ()
;;   (interactive)
;;   (dss/dragon-send-string "natlink.setMicState('sleeping')")
;;   (setq *dss-dragon-locked* t)
;;   (message "dragon locked")
;;   (setq *dss-dragon-state* "sleeping")
;;   (message "dragon locked"))

;; (defun dss/dragon-unlock ()
;;   (interactive)
;;   (setq *dss-dragon-locked* nil))

;; (defun dss/dragon-change-callback (&optional state)
;;   (interactive)
;;   (cond ((and *dss-dragon-locked* (equal state "on"))
;;          (dss/dragon-sleep)
;;          (message "dragon autolocked"))
;;         (t (let ((color-status (if *dss-dragon-locked* "locked" state)))
;;              (progn
;;                ;; this needs refactoring as it's too long and complicated
;;                (dss/dragon-sync-modeline color-status)
;;                (dss/screen-command
;;                 "wrapper"
;;                 (list "eval"
;;                       (format"source ~/tr_toolchain/screen/wrapper-caption-%s"
;;                              (cdr (assoc-string color-status dss-dragon-status-gnu-screen-caption)))))
;;                (call-process-shell-command
;;                 (concat "ssh tavismac ./set-dragon-state.sh '" state "'") nil 0)
;;                (if (equal state "on")
;;                    (dss/screensaver-off))
;;                (call-process-shell-command
;;                 (concat
;;                  "echo '" (cdr (assoc-string color-status dss-dragon-status-gnu-screen-strings))
;;                  "' >> ~/.screen_messages\n") nil 0)
;;                (message "microphone %s" state)
;;                (setq *dss-dragon-state* state)
;;                (if (string-equal *dss-x-display* "b3")
;;                    (let ((aw-color (cdr (assoc-string color-status dss-dragon-status-awesome-colors))))
;;                      (dss/awesome-client
;;                       (format "mywibox[1].fg = \"%s\"; beautiful.border_focus = \"%s\"; client.focus.border_color = beautiful.border_focus"
;;                               aw-color aw-color))))
;;                ;; (dss/awesome-b3-notify (concat "Dragon: " state))
;;                (dss/dragon-log (list :mic-state state))))
;;            )))

;; (defun dss/dragon-get-context ()
;;   (condition-case nil
;;       (let ((context
;;              (cond
;;               ((member major-mode '(lisp-interaction-mode emacs-lisp-mode))
;;                (dss/defun-name))
;;               (t (which-function)))))
;;         (if context
;;             (set-text-properties 0 (length context) nil context))
;;         context)
;;     (error nil)))

;; (defun dss/dragon-log (log-entry)
;;   (interactive)
;;   (save-excursion
;;     (let* ((active-buffer (current-buffer))
;;            (log-buffer (or
;;                         (get-buffer "*dragon-log*")
;;                         (save-window-excursion
;;                           (let ((buf (find-file (concat dss-ephemeral-dir "dragon-log"))))
;;                             (rename-buffer "*dragon-log*")
;;                             buf))))
;;            (line-number (line-number-at-pos))
;;            (column-number (current-column))
;;            (context (dss/dragon-get-context))
;;            (log-list (list
;;                       :time (format-time-string "%Y-%m-%dT%H:%M:%S")
;;                       :buffer (buffer-name active-buffer)
;;                       :line line-number
;;                       :column column-number
;;                       :context context
;;                       :entry log-entry
;;                       )))
;;       (with-current-buffer log-buffer
;;         ;; (get-buffer-create "*dragon*")
;;         (goto-char (point-max))
;;         (insert "\n")
;;         (prin1 log-list log-buffer)))))

;; (defun dss/dragon-results-callback (words)
;;   (interactive)
;;   (if (not (equal *dss-dragon-state* "sleeping"))
;;       (dss/dragon-log (list :voice-command words))))

;; (defun dss/dragon-enable-modeline-sync ()
;;   (interactive)
;;   (setq dss-dragon-mic-timer
;;         (run-with-timer 2 3 'dss/dragon-sync-modeline)))

;; (defun dss/dragon-disable-modeline-sync ()
;;   (interactive)
;;   (cancel-timer dss-dragon-mic-timer)
;;   (set-face-background 'modeline dss-modeline-background-color))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key [(f10)] 'dss/dragon-toggle-mic)
;; ;;(define-key f8-map [(f10)] #'dss/dragon-off)
;; ;;(define-key f8-map [(f9)] #'dss/dragon-lock)

;; (defun dss/say-alex (s)
;;   (interactive "sSay what: ")
;;   (dss/dragon-lock)
;;   (sit-for 0.05)
;;   (do-applescript (format "say \"%s\" using \"alex\" " s)))

(provide 'dss-dragon)


