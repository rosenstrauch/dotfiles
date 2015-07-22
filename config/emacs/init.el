;; User-setting area is below this line.

;;
;; ORG MODE
;;


(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(setq org-agenda-files (quote ("~/org"
"/run/media/rosenstrauch/DATA/DEV/Projectrepos/INTERNAL/rosenstrauch.com/WEBFACTORY/org"
"/run/media/rosenstrauch/DATA/DEV/Projectrepos/INTERNAL/rosenstrauch.com/ROSENREPOS/org"
)))

;;
;; XIKI
;; http://stackoverflow.com/questions/15869029/bind-command-to-c-ret-in-emacs
;; set missing xiki hotkeys https://github.com/trogdoro/xiki/issues/119

(add-to-list 'load-path (expand-file-name "~/.emacs.d/rvm"))
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session


; Load el4r, which loads Xiki
;(add-to-list 'load-path "/home/rosenstrauch/.rvm/gems/ruby-1.9.3-p551/gems/trogdoro-el4r-1.0.10/data/emacs/site-lisp/")
;(require 'el4r)
;(el4r-boot)


; Set default font
(set-face-attribute 'default nil
  :height 110
  :family "Monaco"
  )

; Make tabs into spaces when you type them
(setq-default indent-tabs-mode nil)
; Display existing tabs as 2 characters wide
(setq-default tab-width 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("/home/rosenstrauch/org/agendas.org" "/home/rosenstrauch/org/flagged.org" "/home/rosenstrauch/org/from-mobile.org" "/home/rosenstrauch/org/home.org" "/home/rosenstrauch/org/index.org" "/home/rosenstrauch/org/videoscenes.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
