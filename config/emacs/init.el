;; User-setting area is below this line.

;;
;; Melpa for org-trello
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;
;; Org-trello
;;

(use-package org-trello
  :ensure t)

;; Custom list of trello org files
(custom-set-variables '(org-trello-files '("~/org/boards/Welcome.trello")))

;; org-trello major mode for all .trello files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
              (org-trello-mode)))))
;;
;; ORG MODE
;;

(use-package org
  :ensure t)


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
    ("/home/rosenstrauch/org/*.org"))))

;; ORG-JIRA
(setq jiralib-url "http://acolono.atlassian.net")
;; you need make sure whether the "/jira" at the end is
;; necessary or not, see discussion at the end of this page

(require 'org-jira)
;; jiralib is not explicitly required, since org-jira will load it.


;; Youtrack
(add-to-list 'load-path "~/.emacs.d/youtrack")
(require 'youtrack)
