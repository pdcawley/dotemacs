;;; -*- lexical-binding: t; -*-
;;;
;;; pdcmacs-feeds.el -- Set up various feeds
;;; Primary mastodon at the moment

;; Mastodon does *not* want to play nicely on a tty

(use-package mastodon
  :straight
  (:source melpa)
  :hook
  (mastodon-toot-mode . visual-fill-column-mode)
  :general
  (pdcmacs-app-def "m" 'mastodon)
  (pdcmacs-app-def
    :infix "M"
    "" '(:wk "mastodon")
    "h" '(mastodon-tl--get-home-timeline :wk "Home")
    "@" '(mastodon-notifications--get-mentions :wk "Mentions")
    "t" 'mastodon-toot)

  :init
  (setq-default mastodon-toot--language "en")
  (setq mastodon-instance-url "https://mendeddrum.org"
        mastodon-active-user "pdcawley"
        mastodon-tl--display-media-p window-system
        mastodon-tl--enable-proportional-fonts window-system)
  :config
  (defun ad-mastodon-toot--restore-previous-window-config (window-config)
    (car window-config))
  (advice-add 'mastodon-toot--restore-previous-window-config :before-until 'ad-mastodon-toot--restore-previous-window-config)

  (advice-add 'mastodon-toot--format-attachments
              :before-until #'(lambda () (fboundp 'image-transforms-p)))
  (mastodon-discover))

(provide 'pdcmacs-feeds)
