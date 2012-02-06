(require 'twittering-mode)
(setq twittering-use-master-password t
      twittering-initial-timeline-spec-string
      '(":home"
        ":replies"
        ":favorites"
        ":direct_messages"
        ":search/child of the library/")
      twittering-icon-mode t
      twittering-timer-interval 300
      twittering-url-show-status nil)

(add-hook 'twittering-edit-mode-hook
          (lambda ()
            (and
             (fboundp 'ispell-minor-mode)
             (ispell-minor-mode)
             (fboundp 'flyspell-mode)
             (flyspell-mode))))
