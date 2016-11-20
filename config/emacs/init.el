;;
;; Global Emacs Settings
;;

                                        ; Make tabs into spaces when you type them
(setq-default indent-tabs-mode nil)

                                        ; Display existing tabs as 2 characters wide
(setq-default tab-width 2)
;; load custom lisp from others
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash\\'" . sh-mode))
;; Backups http://pages.sachachua.com/.emacs.d/Sacha.html#org7b1ada1
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(load-theme 'tsdh-dark)
(setq org-fontify-whole-heading-line t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(org-level-1 ((t (:inherit outline-1 :background "blue" :foreground "lavender blush" :box nil :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :box nil :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "magenta" :box nil :height 1.0))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "khaki" :box nil :height 0.9))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "orchid1" :box nil :height 0.8)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Emacs Keyboard remappings                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Melpa and use package                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package is available from here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-variables                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown mode                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export as github markdown http://stackoverflow.com/a/22990257/6768011  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; export as github markdown http://stackoverflow.com/a/22990257/6768011
;;(use-package ox-gfm
;;  :commands (gfm-mode)
;; note conflict with instructions from org-export-backends
;;  :init  (eval-after-load 'org '(require 'ox-gfm))
;;:ensure t)

;; http://stackoverflow.com/questions/21113229/choose-a-different-color-theme-for-printing-in-emacs

(setq org-agenda-exporter-settings '((ps-print-color-p 'black-white)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO habits ttps://github.com/abrochard/emacs-habitica                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package habitica
  :bind (("C-c C-h" . habitica-tasks))
  :ensure t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install Org mode
;; make emacs autorecognize org extension as org major mode
;; remapping C-c ^     (org-sort)

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :load-path "rosenorg-lisp/"
  :bind
  (("\C-c r" . org-sort)
   ("\C-cc" . org-capture)
   ("\C-co" . mby-org-agenda-toggle-list-sublevels))
  :init
  ;; provide a command to show the subtasks org-agenda-dim-blocked-tasks may not be invisible

  (defun mby-org-agenda-toggle-list-sublevels ()
    "Toggle the display of the subtasks in the agenda lists. between nil and t"
    (message "agenda-toggle-list-sublevels")

    (interactive)
    (setq org-agenda-todo-list-sublevels (not org-agenda-todo-list-sublevels)))

  ;; limit wip states
  (defun org-count-todos-in-state (state)
    (let ((count 0))
      (org-scan-tags (lambda ()
                       (when (string= (org-get-todo-state) state)
                         (setq count (1+ count))))
                     t t)
      count))

  (defvar org-wip-limit 2  "Work-in-progress limit")
  (defvar org-wip-state "NEXT")

  (defun org-block-wip-limit (change-plist)
    (message "calling wip limit hook")
    (catch 'dont-block
      (when (or (not (eq (plist-get change-plist :type) 'todo-state-change))
                (not (string= (plist-get change-plist :to) org-wip-state)))
        (throw 'dont-block t))

      (when (>= (org-count-todos-in-state org-wip-state) org-wip-limit )
        (setq org-block-entry-blocking (format "WIP limit: %s" org-wip-state))
        (throw 'dont-block nil))
      t)) ; do not block

  (add-hook 'org-blocker-hook #'org-block-wip-limit)
  :config

  (setq org-wip-limit 2)
  (setq org-wip-state "NEXT")
  ;; Configure org mode Directories
  (setq org-default-notes-file "~/org/home.org")
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2011-10/msg00057.html
  (setq org-agenda-files "~/org")
  (setq org-agenda-files (append '("~/org")
                                 ;;(file-expand-wildcards "~/org/boards/*.trello")

                                 (file-expand-wildcards "~/org/*/*.org")
                                 (file-expand-wildcards "~/org/*/*/*.org")))
  (setq org-archive-location "~/org/04-archive/%s_archive::")

  ;; Mobile org

  ;; Set to the location of your Org files on your local system
  (setq org-directory "~/org")
  ;; Set to the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/org/flagged.org")
  ;; Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")


  ;; setup capture
  (setq org-default-notes-file (concat org-directory "/capture.org"))

  ;; Capture Templates http://orgmode.org/manual/Using-capture.html

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
           "* WISH %?\n  %i\n  %a")
          ;; journal entries do not show up in agenda (but maybe as diary)
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ;; calendar entries prompt for date and show up in agenda may or maynot be todos
          ("c" "Calendar" entry (file+datetree "~/org/calendar.org")
           "* %?\nEntered on %^T\n  %i\n  %a")
          ("h" "Habit" entry (file "~/org/routines.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
          ("l" "Link" entry (file+datetree "~/org/links.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("x" "Firefox Capture Template" entry
       (file+headline "~/org/capture.org" "Firefox")
       "* BOOKMARKS %T\n%c\%a\n%i\n Tan's Note:%?" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)

           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-mode agenda options                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;open agenda in current window
  (setq org-agenda-window-setup (quote current-window))
  ;;warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 2)
  ;;show me tasks scheduled or due in next fortnight
  (setq org-agenda-span (quote fortnight))
  ;;don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

(setq org-agenda-prefix-format '((agenda . " %i %-15:c%?-12t% s")
         (timeline . "  % s")
         (todo .
               "%-15:c")
         (tags .
               " %-12:c %?-12t% s %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
         (search . " %i %-12:c"))
      )

  ;; Tasks mit Datum in der Agenda ausblenden, wenn sie bereits erledigt sind:
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  ;;don't give awarning colour to tasks with impending deadlines
  ;;if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
  ;;don't show tasks that are scheduled or have deadlines in the
  ;;normal todo list
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))
  ;;sort tasks in order of when they are due and then by priority
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
  ;; start with follow mode
  (setq org-agenda-start-with-follow-mode t)

  ;; make agenda view sticky
  (setq org-agenda-sticky t)
  ;; We show only the parent tasks in the agenda's lists…
  ;; https://github.com/bystrano/emacs-conf
  ;;(setq mby-org-agenda-toggle-list-sublevels nil)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down deadline-up time-up priority-down category-keep)
          (todo priority-down todo-state-up category-keep)
          (tags priority-down category-keep)
          (search category-keep)))


  ;; enforce todo dependencies
  (setq org-enforce-todo-dependencies 1)

  ;;(setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-checkbox-dependencies 1)
  ;; http://emacs.stackexchange.com/questions/12364/show-timestamp-for-each-todo-in-org-agenda-global-todo-list
  (setq org-columns-default-format
        "%25ITEM %TODO %5Effort(Time){:} %3PRIORITY $TAGS %TIMESTAMP %6CLOCKSUM(Clock)")
        ; Set default column view headings: Task Effort Clock_Summary


; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

  ;; show all tags available (but only when tagging with : in agenda)
  ;;(setq org-complete-tags-always-offer-all-agenda-tags t)

  ;;
  ;; Clocking
  ;;
  (setq org-agenda-log-mode-items '(state closed clock))

;; so we see how idle timer works switch to agenda after idle timer

;; (defun jump-to-org-agenda ()
;;   (interactive)
;;   (let ((buf (get-buffer "*Org Agenda*"))
;; 	wind)
;;     (if buf
;; 	(if (setq wind (get-buffer-window buf))
;; 	    (select-window wind)
;; 	  (if (called-interactively-p)
;; 	      (progn
;; 		(select-window (display-buffer buf t t))
;; 		(org-fit-window-to-buffer)
;; 		;; (org-agenda-redo)
;; 		)
;; 	    (with-selected-window (display-buffer buf)
;; 	      (org-fit-window-to-buffer)
;; 	      ;; (org-agenda-redo)
;; 	      )))
;;       (call-interactively 'org-agenda-list)))
;;   ;;(let ((buf (get-buffer "*Calendar*")))
;;   ;;  (unless (get-buffer-window buf)
;;   ;;    (org-agenda-goto-calendar)))
;;   )

;;(run-with-idle-timer 25 t 'jump-to-org-agenda)

  ;; auto start tracking default task
  (run-with-idle-timer 25 nil 'bh/clock-in-organization-task-as-default)

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)



(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "a7f3b0f6-77b4-4434-8d03-06a267dc1cc6")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

  ;; Org Diary
  (setq org-agenda-diary-file "~/org/journal.org")

  ;; Custom Agenda http://orgmode.org/worg/sources/org-tutorials/org-custom-agenda-commands.org

  (setq org-agenda-custom-commands
        '(
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          ;; show subtasks with todostate NEXT for headlines tagged PRJ

          ("I" . "Issues") ;; gives label to "I"
          ;; show only Issues
          ("Io" "OPEN Issues" todo "OPEN"
           ((org-agenda-files (file-expand-wildcards "~/org/issues/*/*.issues"))))
         ("B" . "Boards") ;; gives label to "I"
         ;; TODO show only assigned
           ;; show only Issues
           ("Bs" "[s]chedule next Trello Tasks" tags-todo "-SCHEDULED={.+}/!+NEXT" ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))
           ("Ba" "Assigned Trello Tasks" tags "orgtrello\\-users={rosenstrauch}/!+TODO|+WISH|+NEXT|+WAITING|+INSERTED"
            ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))
            ("Bi" "[i]nserted unscheduled Tasks" todo "-SCHEDULED={.+}/!+TODO|+WISH|+NEXT|+WAITING|+INSERTED"
             ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))
          ;; show one next task (actually we want one per project but for now this will have to do)

          ("Q" . "Quests") ;; gives label to "Q"
;; quests should not contain issues and boards?
          ;; match those tagged with :inbox:, are not scheduled, are not DONE. http://stackoverflow.com/a/17004389
          ;; http://emacs.stackexchange.com/a/16561
          ;; http://emacs.stackexchange.com/questions/20155/how-to-show-a-list-of-todo-entries-without-timestamps
          ("Qi" "[i]nserted unscheduled tasks" tags-todo "-SCHEDULED={.+}/!+TODO|+WISH|+NEXT|+WAITING|+INSERTED" )
          ("Qs" "[s]chedule next tasks" tags-todo "-SCHEDULED={.+}/!+NEXT")


          ("Qt" "Do TODAY" agenda "" (
                                      (org-agenda-ndays 1)
                                      (org-agenda-use-time-grid nil)
                                      (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
                                      (org-agenda-view-columns-initially t)))
          ("Qd" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-ndays 1)
            (org-deadline-warning-days 60)
            (org-agenda-time-grid nil)))
          ("F" . "Find - Custom queries/searches") ;; gives label to "Q"
          ("Fi" "Issue search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
          ("FA" "Archive search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
          ;; match tagged headlines that are not todos
          ("K" "Knowledge" tags "+{.*}+TODO=\"\"|+{.*}+TODO=\"DONE\""
           ((org-tags-match-list-sublevels 'indented)
            (org-agenda-sorting-strategy '(tag-up))))
          ;; match orphan headlines (the ones without tag or todo)
          ("O" "Orphans" tags "-{.*}+TODO=\"\""
           ((org-tags-match-list-sublevels 'indented)))
          ("P" . "Projects") ;; gives label to "P"
          ("P1" "Internal Block" (
                         (tags "PRJ" ((org-agenda-overriding-header "\nInternal Projects\n------------------\n")
                                      (org-agenda-files (file-expand-wildcards "~/org/01-internal/*/*")) ))
                         (tags "+{.*}" ((org-agenda-overriding-header "\nInternal Tagged\n------------------\n")
                                      (org-agenda-files (file-expand-wildcards "~/org/01-internal/*/*"))) )
                                   ))
          ("P2" "Customer Agenda and TEAM PRJ-related tasks" (
            (agenda "")
            (tags "PRJ" (
                          (org-agenda-files (append
                                 (file-expand-wildcards "~/org/02-clients/*")
                                 (file-expand-wildcards "~/org/02-clients/*/*.org")))
                                 )
                              (org-agenda-overriding-header "\nActive Team Projects\n------------------\n"))
            (tags "TEAM" (
                          (org-agenda-files (append '("~/org/02-clients")
                                 (file-expand-wildcards "~/org/02-clients/*")
                                 (file-expand-wildcards "~/org/02-clients/*/*.org")))
                          (org-agenda-overriding-header "\nActive Teams\n------------------\n")))))
          ("P6" "Agenda and TEAM PRJ-related tasks" (
            (agenda "")
            (tags "PRJ" (
                          (org-agenda-files (append
                                 (file-expand-wildcards "~/org/06-teams/*")
                                 (file-expand-wildcards "~/org/06-teams/*/*.org")))
                                 )
                              (org-agenda-overriding-header "\nActive Team Projects\n------------------\n"))
            (tags "TEAM" (
                          (org-agenda-files (append '("~/org/06-teams")
                                 (file-expand-wildcards "~/org/06-teams/*")
                                 (file-expand-wildcards "~/org/06-teams/*/*.org")))
                          (org-agenda-overriding-header "\nActive Teams\n------------------\n")))))
          ("Pl" "Refile and stuck" (
                                    (tags "REFILE" )
                                    (stuck "")
                                    (todo "TODO"                                          ;; todos sorted by context
                                          (
                                            ;(org-agenda-prefix-format "%i%e%l%t%s%T:")
                                           (org-agenda-todo-keyword-format "[ ]")
                                           (org-agenda-sorting-strategy '(tag-up priority-down))
                                           (org-agenda-overriding-header "\nTasks by Context\n------------------\n")))))

          ("Pa" "Printed agenda" (
                                  (agenda "" ((org-agenda-ndays 7)
                                              (org-agenda-start-on-weekday nil)
                                              (org-agenda-repeating-timestamp-show-all t)
                                              (org-deadline-warning-days 7)
                                              (org-agenda-todo-keyword-format "[ ]")
(org-agenda-scheduled-leaders '("" ""))
;(org-agenda-entry-types '(:timestamp :sexp))
                                              ))



                                  (todo "TODO"
                                        (
                                         (org-agenda-todo-keyword-format "")
                                         ;(org-agenda-prefix-format  "%i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")

                                        (org-agenda-sorting-strategy '(category-up priority-down))
                                         (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
           ((org-agenda-with-colors nil)
            (org-agenda-compact-blocks t)
            ;;(org-agenda-remove-tags t)
            (ps-number-of-columns 1)
            (org-tags-match-list-sublevels 'indented)
            (ps-landscape-mode nil))
           ("/mnt/DATA/exportedata/org-export/agenda.pdf"))
          ;; Custom Agenda end
          ))
  ;; TODO: unscheduled http://emacs.stackexchange.com/a/868

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; ORG MODE Tags                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        ("@errands" . ?t)
                        ("@BUY" . ?t)
                        ("@meeting" . ?m)
                        ("@phone" . ?c)
                        (:endgroup . nil)
                        ("PRJ" . ?e)
                        ("TEAM" . ?g)
                        ("@laptop" . ?l) ("@pc" . ?p)))

  ;; Controlling tasks http://blog.aaronbieber.com/2016/01/30/dig-into-org-mode.html
  (setq org-log-redeadline (quote time))
  (setq org-log-done (quote time))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-reschedule (quote time))

  ;; TODO Keywords

  (setq org-todo-keywords
        (quote (
                (sequence "WISH(w)"  "TODO(t)" "WAITING(w@/!)" "NEXT(n)" "|" "DONE(d)")
                (sequence "INSERT(i)" "OPEN(o)" "|" "DONE(d)")
                )))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "pink1" :weight bold)
                ("WISH" :foreground "medium sea green" :weight bold)
                ("INSERT" :foreground "medium sea green" :weight bold)
                ("DONE" :foreground "forest green" :weight bold)
                ("WAITING" :foreground "orange" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))


  ;; Project tags http://juanreyero.com/article/emacs/org-teams.html

  (setq org-tags-exclude-from-inheritance '("PRJ")
        org-stuck-projects '("+PRJ/-HOLD-INSERT-DONE"
                             ("NEXT" "TODO") ("@BUY")))

  ;; Org mode keyboard remappings

  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  (bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
  (bind-key "C-c C-w" 'org-refile)

  ;; Refile

  (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 7))))
  (setq org-blank-before-new-entry nil)

  :ensure t)

;;
;; Org-trello
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-trello                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-trello
  ;; org-trello major mode for all .trello files
  :mode (("\\.trello$" . org-mode))
  :ensure t)

;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
                (org-trello-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-JIRA                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; credentials are in authinfo
;; you need make sure whether the "/jira" at the end is
;; necessary or not, see discussion at the end of this page
;; jiralib is not explicitly required, since org-jira will load it.

(use-package org-jira
  :config
  (setq jiralib-url "http://acolono.atlassian.net")

  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GITLAB                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/nlamirault/emacs-gitlab#usage
(unless (package-installed-p 'gitlab)
  (package-install 'gitlab))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAGIT                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  (progn
    (setenv "GIT_PAGER" "")
    (add-hook 'magit-log-edit-mode-hook
              '(lambda ()
                 (auto-fill-mode)
                 (flyspell-mode)
                 (set-fill-column 80)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-projects (publishing)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Switches off use of time-stamps when publishing. I would prefer to publish
;;; everything every time
(setq org-publish-use-timestamps-flag nil)
(setq org-publish-project-alist
      '(("01-internal-html"
         :base-directory "~/org/01-internal/"
         :publishing-directory "/mnt/DATA/exportedata/org_published/01-internal/html"
         :section-numbers nil
         :with-toc nil
         :recursive t
         :publishing-function org-html-publish-to-html)
        ("01-internal-pdf"
         :base-directory "~/org/01-internal/"
         :publishing-directory "/mnt/DATA/exportedata/org_published/01-internal/pdf"
         :section-numbers nil
         :with-toc nil
         :recursive t
         :publishing-function org-latex-publish-to-pdf)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-attach-screenshot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-attach-screenshot
  :bind
  (("C-c S" . org-attach-screenshot)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org sync
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use fork of org sync for id in headline
;; unused because i cannot create issues with this.
(add-to-list 'load-path "~/.emacs.d/org-sync")
(mapc 'load
      '("os" "os-github" "os-bb"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GITHUB                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.issues$" . org-mode))

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

;;(defun gh-issue-create ()
;;  (interactive)
;;  (gh-issue-new-browse (gh-issue-get-project) (org-get-heading) (org-get-entry);))

(defun gh-issue-create ()
  (interactive)
  (gh-issue-new-browse (gh-issue-get-project)
                       (org-get-heading)
                       (org-export-as 'md t)))

(global-set-key (kbd "C-x c g i") 'gh-issue-create)

;; Htmlize for exporting agenda
(use-package htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region)
  :ensure t)

;; try pdf export settings from
;; EXPERIMENTS

                                        ; https://julien.danjou.info/blog/2010/icon-category-support-in-org-mode
(setq org-agenda-category-icon-alist
      '(("[Ee]macs" "/usr/share/icons/hicolor/16x16/apps/emacs.png" nil nil :ascent center)
        ("Naquadah" "~/.emacs.d/icons/org/naquadah.png" nil nil :ascent center)
        ("LR" "~/.emacs.d/icons/LR-icon.png" nil nil :ascent center)
        ("AO" "~/.emacs.d/icons/AO_icon.png" nil nil :ascent center)
        ("Visitors" "~/.emacs.d/icons/org/visitors.png" nil nil :ascent center)
        ("\\(Party\\|Celeb\\)" "~/.emacs.d/icons/org/party.png" nil nil :ascent center)
        ("Wine" "~/.emacs.d/icons/org/wine.png" nil nil :ascent center)
        ("Gnus" "~/.emacs.d/icons/org/gnus.png" nil nil :ascent center)
        ("Org" "~/.emacs.d/icons/org/org.png" nil nil :ascent center)
        ("Medical" "~/.emacs.d/icons/org/medical.png" nil nil :ascent center)
        ("Music" "~/.emacs.d/icons/org/music.png" nil nil :ascent center)
        ("Wii" "~/.emacs.d/icons/org/wii.png" nil nil :ascent center)
        ("Trip" "~/.emacs.d/icons/org/trip.png" nil nil :ascent center)
        ("Train" "~/.emacs.d/icons/org/train.png" nil nil :ascent center)
        ("Anniv" "~/.emacs.d/icons/org/anniversary.png" nil nil :ascent center)
        ("Debian" "~/.emacs.d/icons/org/debian.png" nil nil :ascent center)
        ("Plants" "~/.emacs.d/icons/org/tree.png" nil nil :ascent center)
        ("awesome" "~/.emacs.d/icons/org/awesome.png" nil nil :ascent center)
        ("Solar" "~/.emacs.d/icons/org/solar.png" nil nil :ascent center)
        ("Reading" "~/.emacs.d/icons/org/book.png" nil nil :ascent center)
        ("OpenStack" "~/.emacs.d/icons/org/openstack.png" nil nil :ascent center)
        ("\\(Holidays\\|Vacation\\)" "~/.emacs.d/icons/org/holidays.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

(server-start)
(require 'org-protocol)
