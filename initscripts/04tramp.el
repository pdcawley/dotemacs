;; 04tramp.el -- TRAMP customizations

;; Hopefully will (has?) become unnecessary
(defadvice cd
  (before remove-initial-colon-from-cd activate)
  "Remove an initial colon which is stopping tramp from working"
  (let ((path (ad-get-arg 0)))
    (when (char-equal (string-to-char path) ?:)
      (ad-set-arg 0 (substring path 1)))))
