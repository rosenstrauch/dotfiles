;; User-setting area is below this line.

;; Style


(load-theme 'wheatgrass)


;; Keyboard remappings

;; Duplicate line http://stackoverflow.com/a/88828
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-d") 'duplicate-line)


;;
;; Melpa
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;;
;; Markdown mode
;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))



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
"/DATA/01-INTERNAL/rosencom/WEBFACTORY/org"
"/DATA/01-INTERNAL/rosencom/ROSENREPOS/org"
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

;; setup capture
     (setq org-default-notes-file (concat org-directory "/capture.org"))
     (define-key global-map "\C-cc" 'org-capture)

;;
;; Org-trello
;;

(use-package org-trello
  :ensure t)

;; Custom list of trello org files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(org-agenda-files
   (quote
    ("/home/rosenstrauch/org/agendas.org" "/home/rosenstrauch/org/flagged.org" "/home/rosenstrauch/org/from-mobile.org" "/home/rosenstrauch/org/home.org" "/home/rosenstrauch/org/index.org")))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(org-trello-files (quote ("~/org/boards/Welcome.trello")) nil (org-trello)))

;; org-trello major mode for all .trello files
(add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
              (org-trello-mode)))))
;; ORG-JIRA
;; credentials are in authinfo
(setq jiralib-url "http://acolono.atlassian.net")
;; you need make sure whether the "/jira" at the end is
;; necessary or not, see discussion at the end of this page
(use-package org-jira
  :ensure t)

;; jiralib is not explicitly required, since org-jira will load it.


;; Youtrack
;;(add-to-list 'load-path "~/.emacs.d/youtrack")
;;(require 'youtrack)

;; GITLAB
;; https://github.com/nlamirault/emacs-gitlab#usage
(unless (package-installed-p 'gitlab)
  (package-install 'gitlab))
