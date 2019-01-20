;;; * Use Package php Mode [#2]
(use-package php-mode
  :ensure t)

(use-package phpcbf
  :ensure t
  :init
  (setq phpcbf-executable "~/bin/phpcbf")
  :config
  (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
  (setq phpcbf-standard "Drupal")  )

;; (use-package flymake-php
;;   :ensure t
;;   :config
;;   (add-hook 'php-mode-hook 'flymake-php-load))

;; (use-package flymake-phpcs
;;   :ensure t
;;   :init
;;    (setq flymake-phpcs-standard "Drupal")
;;   :config


;; ;; Customize the coding standard checked by phpcs
;;   (setq flymake-phpcs-standard
;;         "~/.fresh/build/src/PHP_CodeSniffer/CodeSniffer/Standards/Drupal")

;; ;; Show the name of sniffs in warnings (eg show
;; ;; "Generic.CodeAnalysis.VariableAnalysis.UnusedVariable" in an unused
;; ;; variable warning)
;; (setq flymake-phpcs-show-rule t))

(use-package flycheck-phpstan
  :ensure t
  :init
  (defun my-php-mode-hook ()
    "My PHP-mode hook."

    (flycheck-mode t)
    (flycheck-select-checker 'phpstan))

  (add-hook 'php-mode-hook 'my-php-mode-hook)   )
