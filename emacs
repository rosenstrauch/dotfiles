

;; User-setting area is below this line.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rvm"))
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

(put 'upcase-region 'disabled nil)
; Load el4r, which loads Xiki
(add-to-list 'load-path "~/.rvm/gems/ruby-1.9.3-p551/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
(require 'el4r)
(el4r-boot)
(el4r-troubleshooting-keys)
