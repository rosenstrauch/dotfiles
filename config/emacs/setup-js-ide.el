
;;; * Use Package js2-mode [#2]
(use-package prettier-js
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode)
  (setq prettier-js-args '(
                           "--trailing-comma" "all"
                           "--bracket-spacing" "false"
                           "--no-semi"
                           "--single-quote"
                           ))
  :ensure t  )


(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :ensure t
  :hook ((js2-mode . (lambda ()
                       (flycheck-mode)
                       ))
         (js2-jsx-mode . (lambda ()
                           (flycheck-mode)
                           )))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
        js-chain-indent t))


;;; * Use Package indium: javascript awesome development environment
;;;;   - https://github.com/NicolasPetton/indium
(use-package indium
  :ensure t
  :after js2-mode
  :bind (:map js2-mode-map
              ("C-c C-l" . indium-eval-buffer)
              ("C-c C-n" . indium-run-node)
              ("C-c b l" . indium-list-breakpoints)
              ("C-c b K" . indium-remove-all-breakpoints-from-buffer)
              ("C-c b t" . indium-toggle-breakpoint))
  :hook ((js2-mode . indium-interaction-mode)))

(use-package js-auto-beautify
  :ensure t)

(use-package js-import
  :ensure t)

