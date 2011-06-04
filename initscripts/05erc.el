(require 'erc)
(require 'erc-track)

;; It seems that point 2 "The user is not interested in customizing it" in the
;; original documentation comment is wrong. I want mouse-1 to switch in place,
;; and mouse-3 to switch in another buffer. 
(defun erc-make-mode-line-buffer-name (string buffer &optional faces count)
  "Return STRING as a button that switches to BUFFER when clicked.
If FACES are provided, color STRING with them."
  ;; We define a new sparse keymap every time, because 1. this data structure
  ;; is very small, the alternative would require us to defvar a keymap,
  ;; 2. the user is not interested in customizing it (really? (Definitely not,
  ;; pdc), 3. the defun needs to switch to BUFFER, so we would need to save
  ;; that value somewhere.
  (let ((map (make-sparse-keymap))
        (name (if erc-track-showcount
                  (concat string
                          erc-track-showcount-string
                          (int-to-string count))
                (copy-sequence string))))
    (define-key map (vector 'mode-line 'mouse-1)
      `(lambda (e)
         (interactive "e")
         (save-selected-window
           (select-window
            (posn-window (event-start e)))
           (switch-to-buffer ,buffer))))
    (define-key map (vector 'mode-line 'mouse-3)
      `(lambda (e)
         (interactive "e")
         (save-selected-window
           (select-window
            (posn-window (event-start e)))
           (switch-to-buffer-other-window ,buffer))))
    (put-text-property 0 (length name) 'local-map map name)
    (put-text-property
     0 (length name)
     'help-echo (concat "mouse-1: switch to buffer, "
                        "mouse-3: switch to buffer in other window")
     name)
    (put-text-property 0 (length name) 'mouse-face erc-track-mouse-face name)
    (when (and faces erc-track-use-faces)
      (put-text-property 0 (length name) 'face faces name))
    name))

;; Nicked from Jon Rockway's elisp repo
;; http://github.com/jrockway/elisp/tree/master/_local/erc-extras.el

(defun erc-track-reset ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update)
  (erc-modified-channels-display)
  (force-mode-line-update t))

(defun erc-go-away nil
  (interactive)
  (loop for buf in (buffer-list)
        when (with-current-buffer buf (eq 'erc-mode major-mode))
        do (bury-buffer buf)))

(defun erc-next-channel (no-select-1)
  (interactive "P")
  (if (not no-select-1) (window-number-select 1))
  (let ((buffer (car erc-modified-channels-alist)))
    (when (not buffer)
      (error "No more buffers!"))
    (switch-to-buffer (car buffer))
    (goto-char (point-max))))


(setq nick-face-list '())

;; Define the list of colors to use when coloring IRC nicks.
(setq-default erc-colors-list
              '("dark orange" "spring green" "dark violet" "magenta"
                "firebrick" "salmon" "light salmon" "rosy brown" "chocolate"
                "goldenrod" "dark khaki"
                "olive drab" "green yellow" "light green" "lime green" "pale green"
                "medium sea green" "yellow" "royal blue" "slate blue"
                "medium purple" "blue violet" "medium violet red" "hot pink"))

(defun build-nick-face-list ()
  "build-nick-face-list builds a list of new faces using the
foreground colors specified in erc-colors-list.  The nick faces
created here will be used to format IRC nicks."
  (setq i -1)
  (setq nick-face-list
        (mapcar
         (lambda (COLOR)
           (setq i (1+ i))
           (list (custom-declare-face
                  (make-symbol (format "erc-nick-face-%d" i))
                  (list (list t (list :foreground COLOR)))
                  (format "Nick face %d" i))))
         erc-colors-list)))

(defun my-erc-colorize-nick ()
  "This insert-modify hook looks for nicks in new messages and
computes md5(nick) and uses substring(md5_value, 0, 4) mod (length
nick-face-list) to index the face list and produce the same face for a
given nick each time it is seen.  We get a lot of collisions this way,
unfortunately, but it's better than some other methods I tried.
Additionally, if you change the order or size of the erc-colors-list,
you'll change the colors used for nicks."
  (if (null nick-face-list) (build-nick-face-list))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "\\(<\\)\\([^>]*\\)\\(>\\)")
        (let ((nick (match-string 2)))
          (loop for n in '(1 3) do
                (put-text-property (match-beginning n) (match-end n)
                                   'face (nth
                                          (mod (string-to-number
                                                (substring (md5 nick) 0 4) 16)
                                               (length nick-face-list))
                                          nick-face-list)))))))

;; This adds the ERC message insert hook.
(add-hook 'erc-insert-modify-hook 'my-erc-colorize-nick)

;; Setup the gear to connect to forge irc.
(require 'tls)
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.dev.bbc.co.uk"
           :port 6697
           :nick "PiersCawley"
           :full-name "Piers Cawley")
  (erc     :server "irc.perl.org" :port 6667)
  (erc     :server "irc.filknet.org" :port 6667)
  (erc     :server "irc.runstate.com" :port 6667)
  (erc     :server "irc.freenode.net" :port 6667))
