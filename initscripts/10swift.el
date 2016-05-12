(use-package swift-mode
  :ensure t
  :init
  (progn
    (setq flycheck-swift-sdk-path nil
          flycheck-swift-linked-sources nil
          flycheck-swift-framework-search-paths '("." "~/Library/Frameworks")
          flycheck-swift-cc-include-directories nil
          )
    ))
