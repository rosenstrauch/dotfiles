(global-set-key (kbd "C-d") 'duplicate-line)

(global-set-key (kbd "\C-c gi") 'find-user-init-file)
(global-set-key (kbd "\C-c M-_") 'describe-personal-keybindings)
(global-set-key (kbd "\C-c go") 'browse-url-at-point)
(global-set-key (kbd "\C-c t") 'my-start-ansi-bash)
(global-set-key (kbd "\C-c gO") 'my/find-org-file)

(global-set-key (kbd "\C-c gx") 'org-wiki-index)
(global-set-key (kbd "\C-c gw") 'org-wiki-panel)
(global-set-key (kbd "\C-x RET RET") 'my/go-home)


;; use org links anywhere
(global-set-key "\C-cL" 'org-insert-link-global)
(global-set-key "\C-co" 'org-open-at-point-global)
(global-set-key "\C-cl" 'org-store-link)

(global-set-key "\C-xf" 'fasd-find-file)

;; Status
;;(global-set-key "\C-cSi" 'ifconfig)

;;(global-set-key "\C-cSr" 'route)

(global-unset-key "\C-xz")

(global-unset-key "\C-x=")
(global-unset-key "\C-x+")
(global-unset-key "\C-x-")

(global-unset-key "\C-x#")

(global-unset-key "\C-x^")
(global-unset-key "\C-x$")
(global-unset-key "\C-x<")
(global-unset-key "\C-x>")
(global-unset-key "\C-x[")
(global-unset-key "\C-x]")
(global-unset-key "\C-x;")
(global-unset-key "\C-x.")
(global-unset-key "\C-x*")
(global-unset-key "\C-x}")
(global-unset-key "\C-x{")
