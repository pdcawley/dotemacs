;;; We use a combination of ubiquitous IDO, ibuffer and smex to handle various
;;; 'completing read' situations. So... set 'em all up here.

(use-package ido
  :bind
  ("C-x 5 t" . ido-switch-buffer-tiny-frame)
  :defines
  (ido-cur-item ido-require-match ido-selected ido-final-text
                ido-show-confirm-message ido-show-dot-for-dired)
  :init
  (progn
    (setq ido-show-dot-for-dired t
          ido-use-faces t
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess)
    (ido-mode 'buffer))
  :config
  (progn
    (use-package ido-better-flex
      :config
      (ido-better-flex/enable))

    (use-package ido-complete-space-or-hyphen
      :config
      (ido-complete-space-or-hyphen-enable))

    (use-package ido-vertical-mode
      :ensure t
      :config
      (ido-vertical-mode))

    (use-package ido-ubiquitous
      :ensure t
      :config
      (ido-ubiquitous-mode t))

    (use-package ido-hacks
      :disabled t
      :init
      (ido-hacks-mode 1))

    (use-package projectile
      :ensure t
      :diminish projectile-mode
      :bind
      ("C-c f" . projectile-find-file)
      :init
      (setq projectile-enable-caching nil projectile-globally-ignored-directories 'target)
      :config
      (progn
        (projectile-global-mode)
        (defadvice projectile-project-root
            (after projectile-project-root-has-final-slash activate)
          (setq ad-return-value (replace-regexp-in-string "/?$" "/" ad-return-value)))))

    (defun ido-smart-select-text nil
      "Select the current completed item. Do NOT descend into directories."
      (interactive)
      (when
          (and (or (not ido-require-match)
                   (if (memq ido-require-match
                             '(confirm confirm-after-completion))
                       (if (or
                            (eq ido-cur-item 'dir)
                            (eq last-command this-command))
                           t
                         (setq ido-show-confirm-message t)
                         nil))
                   (ido-existing-item-p))
               (not ido-incomplete-regexp))
        (when ido-current-directory
          (setq ido-exit 'takeprompt)
          (unless (and ido-text
                       (= 0 (length ido-text)))
            (let ((match (ido-name (car ido-matches))))
              (throw 'ido
                     (setq ido-selected (if match
                                            (replace-regexp-in-string "/\\'" "" match)
                                          ido-text)
                           ido-text ido-selected
                           ido-final-text ido-text)))))
        (exit-minibuffer)))

    (defun ido-switch-buffer-tiny-frame (buffer)
      (interactive (list (ido-read-buffer "Buffer: " nil t)))
      (with-selected-frame
          (make-frame
           '((width . 80)
             (height . 22)
             (left-fringe . 0)
             (right-fringe . 0)
             (vertical-scroll-bars)
             (unsplittable . t)
             (has-modeline-p)
             (minibuffer)))
        (switch-to-buffer buffer)
        (set (make-local-variable 'mode-line-format) nil)))))

(use-package ibuffer
  :defer t
  :bind ("C-x C-b" . ibuffer)
  :init
  (add-hook 'ibuffer-mode-hook
            #'(lambda ()
                (ibuffer-switch-to-saved-filter-groups "default"))))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))


