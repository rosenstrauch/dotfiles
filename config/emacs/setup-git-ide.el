(use-package magit
  :commands (magit-status magit-blame magit-mode)
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c C-g c" . magit-commit)
         ("C-c C-g f" . magit-grep))
  :config
  (progn
    ;; Set `magit-status' fullscreen
    (setq magit-post-display-buffer-hook
          #'(lambda ()
              (when (derived-mode-p 'magit-status-mode)
                (delete-other-windows))))

    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80))))

  (use-package magit-blame
    :bind
    ("C-c C-g b" . magit-blame-mode)))

(use-package magit-gh-pulls
  :diminish magit-gh-pulls-mode
  :ensure t
  :disabled t
  :commands (magit-gh-pulls-mode turn-on-magit-gh-pulls))

(use-package magithub
  :after magit
  :ensure t
  :disabled t
  :config (magithub-feature-autoinject t))

(use-package git-gutter
  :defer 0.3
  :diminish
  :ensure t
  :init (global-git-gutter-mode +1))

(use-package git-timemachine
  :defer 1
  :diminish
  :ensure t)

(use-package forge
  :after magit
  :ensure t)
