;; User-setting area is below this line.

;; Global Emacs Settings

; Make tabs into spaces when you type them
(setq-default indent-tabs-mode nil)

; Display existing tabs as 2 characters wide
(setq-default tab-width 2)
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;; Backups http://pages.sachachua.com/.emacs.d/Sacha.html#org7b1ada1
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;
;; Style
;;

(load-theme 'tsdh-dark)
(setq org-fontify-whole-heading-line t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :background "blue" :foreground "lavender blush" :box nil :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :box nil :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "magenta" :box nil :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "khaki" :box nil :height 0.9))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "orchid1" :box nil :height 0.8)))))

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

;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package now


;;
;; Markdown mode
;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))



;; export as github markdown http://stackoverflow.com/a/22990257/6768011

(use-package ox-gfm
  :commands (gfm-mode)
  :init  (eval-after-load 'org '(require 'ox-gfm)) 
  :ensure t)

;;
;; TODO habits
;; https://github.com/abrochard/emacs-habitica
;;

(use-package habitica
  :ensure t
  
  )



;;
;; ORG MODE
;;

;; make emacs autorecognize org extension as org major mode

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))


;; Install Org mode (can we add all of the following config inside this use package declaration?)
(use-package org
  :ensure t)

;; Configure org mode Directories
(setq org-default-notes-file "~/org/home.org")
;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00057.html
(setq org-agenda-files "~/org")
(setq org-agenda-files (append '("~/org")
(file-expand-wildcards "~/org/boards/*.trello")
(file-expand-wildcards "~/org/issues/*.issues")
(file-expand-wildcards "~/org/*/*.org")
))

(setq org-archive-location "~/org/04-archive/%s_archive::")


;; Agenda and tasks
;; enforce todo dependencies
(setq org-enforce-todo-dependencies 1)
(setq org-agenda-dim-blocked-tasks 1)
(setq org-enforce-todo-checkbox-dependencies 1)
; http://emacs.stackexchange.com/questions/12364/show-timestamp-for-each-todo-in-org-agenda-global-todo-list
(setq org-columns-default-format
      "%25ITEM %TODO %3PRIORITY $TAGS %TIMESTAMP %5Effort(Time){:} %6CLOCKSUM(Clock)")


;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;; Custom Agenda http://orgmode.org/worg/sources/org-tutorials/org-custom-agenda-commands.org

(setq org-agenda-custom-commands

      '(
        ("x" agenda)
        ;; show subtasks with todostate NEXT for headlines tagged PRJ
        ("P" . "Projects") ;; gives label to "P"
        ("Pl" "Projects List" tags "PRJ")
        ;; second try to show only next tasks for projects (only works if parent project is a task with child tasks)
        ("Pn" "Project NEXT" tags "PRJ/NEXT")
        ("I" . "Issues") ;; gives label to "I"
        ;; show only Issues
        ("In" "NEXT Issues" todo "NEXT"
         ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
        ;("i" "Incubating Projects" org-tags-view "PRJ/!+MAYBE|+INSERT|+WISH" nil);; incubating projects

        ;("y" agenda*)
        ;; show one next task (actually we want one per project but for now this will have to do)
        ("Q" . "Quests") ;; gives label to "Q"
        ("Qn" "Next Tasks" todo "NEXT")
        ("Qc" "Current Task" todo "NEXT"
         ((org-agenda-max-todos 2)
          (org-agenda-view-columns-initially t)))
        ;("H" todo-tree "NEXT") ; only current file
        ;("w" "wishes" todo-tree "WISH");; wishes
        ;("u" tags "+@home-urgent")
        ;("v" tags-todo "+@home-urgent")
        ;("U" tags-tree "+@home-urgent")
        ;("f" occur-tree "\\<FIXME\\>")
        
        ("F" . "Find - Custom queries/searches") ;; gives label to "Q"
        ("Fi" "Issue search" search ""
         ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
        ("FA" "Archive search" search ""
         ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
        ("Fs" "published search" search ""
         ((org-agenda-files (file-expand-wildcards "~/org/08-pubsys/*.org"))))
        ("Fb" "published and Archive" search ""
         ((org-agenda-text-search-extra-files (file-expand-wildcards "~/archive/*.org_archive"))))
        ;; searches both projects and archive directories
        ("FA" "Archive tags search" org-tags-view ""
         ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))

        
        ;; ...other commands here
        ;; match orphan headlines (the ones without tag or todo)
        ("O" "Orphans" tags "-{.*}+TODO=\"\"")
        ;; match those tagged with :inbox:, are not scheduled, are not DONE. http://stackoverflow.com/a/17004389
        ;; http://emacs.stackexchange.com/a/16561
        ;; http://emacs.stackexchange.com/questions/20155/how-to-show-a-list-of-todo-entries-without-timestamps
        ("Ti" "[i]nserted unscheduled tasks" tags-todo "-SCHEDULED={.+}/!+TODO|+STARTED|+WAITING|+INSERTED")
        ("Ts" "[s]chedule next tasks" tags-todo "-SCHEDULED={.+}/!+NEXT")
        ("Th" "Agenda and Home-related tasks"
         ((agenda "")
          (tags-todo "home")
          (tags "garden")))
        ("To" "Agenda and Office-related tasks"
         ((agenda "")
          (tags-todo "work")
          (tags "office")))
;; Custom Agenda end
))

;; TODO: unsceduled http://emacs.stackexchange.com/a/868


;; Tags

(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      ("@errands" . ?t)
                      ("@BUY" . ?t)
                      ("@meeting" . ?m)
                      ("@phone" . ?c)
                      (:endgroup . nil)
                      ("PRJ" . ?e)
                      ("@laptop" . ?l) ("@pc" . ?p)))

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
              ("NEXT" :foreground "pink1" :weight bold)
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

(setq org-tags-exclude-from-inheritance '("PRJ")
      org-stuck-projects '("+PRJ/-HOLD-INSERT-DONE"
                           ("NEXT") ("@BUY")))

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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(inhibit-startup-screen t)
 '(org-export-backends (quote (ascii html icalendar latex md odt gfm)))
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello))
 '(package-selected-packages
   (quote
    (helm-github-stars gitlab org-jira org-trello use-package markdown-mode habitica))))
;;
;; Org-trello
;;

(use-package org-trello
  :ensure t)

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

;; GITHUB
(add-to-list 'auto-mode-alist '("\\.issues$" . org-mode))

;; use fork of org sync for id in headline
;; unused because i cannot create issues with this.
(add-to-list 'load-path "~/.emacs.d/org-sync")
(mapc 'load
      '("org-sync" "org-sync-bb" "org-sync-github"))
;;(setq org-sync-id-in-headline 1)


;;(use-package gist
;;  :ensure t)
;;(use-package github-notifier
;;  :ensure t)
;;(use-package github-issues
;;  :ensure t
;;  :pin manual)
;;(use-package helm-github-issues
;;  :ensure t
;;  :pin manual)
;;(use-package org-github-links
;;  :ensure t
;;  :pin manual)


;; (use-package helm-github-stars
;;  :ensure t)

;; http://moritz-breit.de/blog/2015/10/05/github-issues-in-emacs/
(defun gh-issue-new-url (project title body)
  (concat "https://github.com/"
          project
          "/issues/new?title="
          (url-hexify-string title)
          "&body="
          (url-hexify-string body)))

(defun gh-issue-new-browse (project title body)
  (browse-url (gh-issue-new-url project title body)))

(defun gh-issue-get-project ()
  (org-entry-get (point) "GH-PROJECT" t))

                                        ;(defun gh-issue-create ()
                                        ;  (interactive)
                                        ;  (gh-issue-new-browse (gh-issue-get-project) (org-get-heading) (org-get-entry);))

(defun gh-issue-create ()
  (interactive)
  (gh-issue-new-browse (gh-issue-get-project)
                       (org-get-heading)
                       (org-export-as 'md t)
                       ))

(global-set-key (kbd "C-x c g i") 'gh-issue-create)
