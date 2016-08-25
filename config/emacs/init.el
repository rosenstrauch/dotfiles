;; User-setting area is below this line.

;; Global Emacs Settings

; Make tabs into spaces when you type them
(setq-default indent-tabs-mode nil)
; Display existing tabs as 2 characters wide
(setq-default tab-width 2)

;; Backups http://pages.sachachua.com/.emacs.d/Sacha.html#org7b1ada1
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;
;; Style
;;

(load-theme 'wheatgrass)

;;
;; Global Emacs Keyboard remappings
;;

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



;; http://pages.sachachua.com/.emacs.d/Sacha.html#orgb362383
(with-eval-after-load 'org
     (bind-key "C-c k" 'org-cut-subtree org-mode-map)
     (setq org-yank-adjusted-subtrees t))
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

;; make emacs autorecognize org extension as org major mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Install Org mode
(use-package org
  :ensure t)

;; Configure org mode Directories
(setq org-default-notes-file "~/org/home.org")
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00057.html
(setq org-agenda-files "~/org")
(setq org-agenda-files (file-expand-wildcards "~/org/*/*.trello" "~/org/*/*.org"))
(setq org-archive-location "~/org/04-archive/%s_archive::")


;; Agenda
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;; Custom Agenda http://orgmode.org/worg/sources/org-tutorials/org-custom-agenda-commands.org
(setq org-agenda-custom-commands

      '(("Q" . "Custom queries") ;; gives label to "Q"
        ;; wishes
  ("Qw" "wishes" tags-todo "TODO=\"WISH\" ")
  ("Qm" "maybe search" todo "MAYBE") ;; review someday/maybe items
  ("Qp" "Projects" tags "PRJ");; review project items
  ("QP" "project search" org-tags-view "PRJ")
	("Qa" "Archive search" search ""
	 ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
	("Qs" "published search" search ""
	 ((org-agenda-files (file-expand-wildcards "~/org/08-pubsys/*.org"))))
	("Qb" "published and Archive" search ""
	 ((org-agenda-text-search-extra-files (file-expand-wildcards "~/archive/*.org_archive"))))
	        ;; searches both projects and archive directories
	("QA" "Archive tags search" org-tags-view ""
	 ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))

  
	;; ...other commands here
  
	 ))

	 ;; Tags

     (setq org-tag-alist '((:startgroup . nil)
                           ("@work" . ?w) ("@home" . ?h)
                           ("@errands" . ?t)
                           ("@meeting" . ?m)
                           ("@phone" . ?c)
                           (:endgroup . nil)
                           ("@PRJ" . ?e)
                           ("laptop" . ?l) ("pc" . ?p)))

;; Controlling tasks http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
(setq org-log-redeadline (quote time))
(setq org-log-done (quote time))
(setq org-enforce-todo-dependencies t)
(setq org-log-reschedule (quote time))

;; TODO Keywords

(setq org-todo-keywords
      (quote ((sequence "WISH(w)" "INSERT(i)" "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("WISH" :foreground "medium sea green" :weight bold)
              ("INSERT" :foreground "medium sea green" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("MEETING" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; setup capture
     (setq org-default-notes-file (concat org-directory "/capture.org"))
     (define-key global-map "\C-cc" 'org-capture)

;; Capture Templates http://orgmode.org/manual/Using-capture.html

     (setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("c" "Calendar" entry (file+datetree "~/org/calendar.org")
         "* %?\nEntered on %U\n  %i\n  %a")
        ("l" "Link" entry (file+datetree "~/org/links.org")
                     "* %?\nEntered on %U\n  %i\n  %a")
             ))

;; Project tags http://juanreyero.com/article/emacs/org-teams.html

(setq org-tags-exclude-from-inheritance '("@PRJ")
      org-stuck-projects '("+@PRJ/-MAYBE-DONE"
                           ("TODO" "TASK") ()))

;; Org mode keyboard remappings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
(bind-key "C-c C-w" 'org-refile)

;; Refile

(setq org-refile-targets
      '(
      ("~/org/home.org" :maxlevel . 4)
        ((file-expand-wildcards "~/org/*/*.org") :maxlevel . 3)
        ((file-expand-wildcards "~/org/*/*/*.org") :maxlevel . 3)
        ))
(setq org-reverse-note-order t)
(setq org-refile-use-outline-path nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-use-cache nil)
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-blank-before-new-entry nil)


;; Mobile org

;; Set to the location of your Org files on your local system
(setq org-directory "~/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

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

 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))

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

;; GITLAB
;; https://github.com/nlamirault/emacs-gitlab#usage
(unless (package-installed-p 'gitlab)
  (package-install 'gitlab))
