;;; * GLOBAL Emacs Settings
;;; ** GLOBAL Load custom lisp from others
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
;;; ** GLOBAL Configure Backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;; ** GLOBAL KEYMAPs
;; Set the minor mode prefix to C-co
(setq outline-minor-mode-prefix "\C-o")
;;; *** KEYMAP Next and previous page
(define-key prog-mode-map "\C-x\C-n" #'forward-page)
(define-key prog-mode-map "\C-x\C-p" #'backward-page)
;;; *** KEYMAP Duplicate line C-d
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
;;; *** KEYMAP Cut Subtree C-c k
(with-eval-after-load 'org
  (bind-key "C-c k" 'org-cut-subtree org-mode-map)
  (setq org-yank-adjusted-subtrees t))
;;; *** KEYMAP quick access to init file C-c I
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file)
;;; *** KEYMAP TODO quick access to home.org
;;; ** STYLE
;;; *** STYLE: Autorecognize Page Breaks
(setq page-delimiter
      (rx bol (or "\f" ";;;")
          (not (any "#")) (* not-newline) "\n"
          (* (* blank) (opt ";" (* not-newline)) "\n")))
;; Expanded regexp:
;; "^;;;[^#].*\n\\(?:[[:blank:]]*\\(?:;.*\\)?\n\\)*"
;;; *** STYLE: Make tabs into spaces when you type them
(setq-default indent-tabs-mode nil)
;;; *** STYLE: Display existing tabs as 2 characters wide
(setq-default tab-width 2)
(load-theme 'tsdh-dark)
(setq org-fontify-whole-heading-line t)
(set-default-font "9x15")
(line-number-mode 1)
(column-number-mode 1)
(setq line-spacing '0.25)
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

;;; * BOOTSTRAP Package Management
;;; ** BOOTSTRAP package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
;;; ** BOOTSTRAP `use-package'
(unless (package-installed-p 'use-package)
;;; TODO: dont run this on every start but dont not run it so we dont have outdated lists.
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
;;; use-package is available from here on

;;; * Use Package: ORG MODE

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :load-path "rosenorg-lisp/"
;;; ** ORG KEYBINDINGS
;;; C-c ^ | (org-sort)
;;; C-c c | capture
;;; C-c +  | toggle sublevels (needs reload)
  :bind
  (("\C-c r" . org-sort)
   ("\C-cc" . org-capture)
   ("\C-c +" . mby-org-agenda-toggle-list-sublevels))
;;; ** ORG INIT
  :init
;;; *** ORG INIT: allow linking by id
  (setq org-id-link-to-org-use-id t)
;;; *** ORG INIT: provide a command to show the subtasks
;;; *** ORG INIT: NOTE: org-agenda-dim-blocked-tasks may not be invisible

  (defun mby-org-agenda-toggle-list-sublevels ()
    "Toggle the display of the subtasks in the agenda lists. between nil and t"
    (message "agenda-toggle-list-sublevels")

    (interactive)
    (setq org-agenda-todo-list-sublevels (not org-agenda-todo-list-sublevels)))
;;; *** ORG INIT: Provide Hook and Function to Limit amount of tasks in NEXT state per project

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

;;; ** ORG CONFIG: Icons

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





  ;; configure wip limit to 2 next tasks
  (setq org-wip-limit 2)
  (setq org-wip-state "NEXT")
;;; *** ORG CONFIG: Directories
  (setq org-default-notes-file "~/org/home.org")
  (setq org-agenda-files "~/org")
  (setq org-agenda-files (append '("~/org")
                                 ;;(file-expand-wildcards "~/org/boards/*.trello")
                                 (file-expand-wildcards "~/org/*/*.org")
                                 (file-expand-wildcards "~/org/*/*/*.org")))
  (setq org-archive-location "~/org/04-archive/%s_archive::")

;;; *** ORG CONFIG Mobile org
;;; **** ORG MOBILE: the location of your Org files on your local system
  (setq org-directory "~/org")
;;; **** ORG MOBILE: the name of the file where new notes will be stored
  (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;;; **** ORG MOBILE: Set to <your Dropbox root directory>/MobileOrg.
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

;;; *** ORG CONFIG: CAPTURE

  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
           "* WISH %?\n  %i\n  %a")
;;; **** CAPTURE: Ideas are not tasks                                           
          ("i" "Idea" entry (file+headline "~/org/ideas.org" "IdeaInbox")
           "* %?\nEntered on %U\n  %i\n  %a")   
;;; **** CAPTURE: journal entries do not show up in agenda (but maybe as diary)
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
;;; **** CAPTURE: calendar entries prompt for date and show up in agenda may or maynot be todos
          ("c" "Calendar" entry (file+datetree "~/org/calendar.org")
           "* %?\nEntered on %^T\n  %i\n  %a")
;;; **** CAPTURE: Habits are repeated tasks captured to routines.org
          ("h" "Habit" entry (file "~/org/routines.org")
           "** TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
;;; **** CAPTURE: LinkLibrary is sorted by capture date in a date tree
          ("l" "Link" entry (file+datetree "~/org/links.org")
           "* %c %a %x %?\nEntered on %U\n  %i\n" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)
;;; **** CAPTURE: Snippets will need to get refiled if i capture a link it may include a snipptet
          ("x" "Firefox Capture Template" entry
           (file+headline "~/org/capture.org" "Firefox")
           "* Snippets %a\n%i\nEntered on %U\n%c\ \nNote: %?\nLink: %l" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)

          ))

;;; *** ORG CUSTOM AGENDA Views

  (setq org-agenda-custom-commands
        '(
;;; **** CUSTOM AGENDA: Priority views
          ("c" . "Priority views")
          ("ca" "#A"  (
                       (tags "PRIORITY=\"A\""
                             ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                              (org-agenda-overriding-header "High-priority unfinished tasks:")))
                       (alltodo ""
                                ((org-agenda-entry-types '(:scheduled))
                                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
                                                                                      "\\[#A\\]"))))))
          ("cb" "#B" alltodo ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
                                                                 "\\[#B\\]"))))
          ("cc" "#C" alltodo ""
           ((org-agenda-entry-types '(:scheduled))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
                                                                 "\\[#C\\]"))))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
;;; **** CUSTOM AGENDA: Issues

          ("I" . "Issues") ;;; gives label to "I"
;;; ***** shows open tasks from .issue files
          ("Io" "OPEN Issues" todo "OPEN"
           ((org-agenda-files (file-expand-wildcards "~/org/issues/*/*.issues"))))
;;; **** CUSTOM AGENDA: Boards
          ("B" . "Boards") ;;; gives label to "I"
;;; ***** TODO show only assigned

          ("Bs" "[s]chedule next Trello Tasks" tags-todo "-SCHEDULED={.+}/!+NEXT" ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))
          ("Ba" "Assigned Trello Tasks" tags "orgtrello\\-users={rosenstrauch}/!+TODO|+WISH|+NEXT|+HOLD|+INSERTED"
           ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))
          ("Bi" "[i]nserted unscheduled Tasks" todo "-SCHEDULED={.+}/!+TODO|+WISH|+NEXT|+HOLD|+INSERTED"
           ((org-agenda-files (file-expand-wildcards "~/org/boards/*.trello"))))

;;; **** CUSTOM AGENDA: Quests
          ("Q" . "Quests") ;;; gives label to "Q"
;;; ***** quests dont contain issues and boards
;;; ***** quests that are estimated, are not scheduled, are not DONE.
;;; ***** quests to estimate next
          ("Qi" "[i]nserted unestimated tasks" tags-todo "Effort<1-SCHEDULED={.+}/!-DONE" )
;;; ***** quests which have estimates
          ("Qs" "[s]chedule next tasks" tags-todo "Effort>1-SCHEDULED={.+}/!-DONE")
;;; ***** quests scheduled for today
          ("Qt" "Do TODAY" agenda ""
           ((org-agenda-ndays 1)
            (org-agenda-use-time-grid nil)
            (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
            (org-agenda-view-columns-initially t)))
          ("Qd" "Upcoming deadlines" agenda ""
           ((org-agenda-entry-types '(:deadline))
            (org-agenda-ndays 1)
            (org-deadline-warning-days 60)
            (org-agenda-time-grid nil)))
;;; **** CUSTOM AGENDA: queries/searches
          ("F" . "Find - Custom queries/searches") ;;; gives label to "Q"
;;; ***** sparse tree for next string
          ("Fn" "Next in file" occur-tree "NEXT")
          ("Fi" "Issue search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
          ("FA" "Archive search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
;;; ***** match tagged headlines that are not todos
          ("K" "Knowledge" tags "+{.*}+TODO=\"\"|+{.*}+TODO=\"DONE\""
           ((org-tags-match-list-sublevels 'indented)
            (org-agenda-sorting-strategy '(tag-up))))
;;; ***** match orphan headlines (the ones without tag or todo)
          ("O" "Orphans" tags "-{.*}+TODO=\"\""
           ((org-tags-match-list-sublevels 'indented)))
          ("P" . "Projects") ;;; gives label to "P"
          ("P1" "Internal Block"
           ((tags "PRJ" ((org-agenda-overriding-header "\nInternal Projects\n------------------\n")))
            (tags-todo "Effort>1-SCHEDULED={.+}/!-DONE" ((org-agenda-overriding-header "\Estimated Unscheduled Internal Tasks\n------------------\n"))))
;;; ***** only look in internal org files
           ((org-agenda-files (file-expand-wildcards "~/org/01-internal/*/*"))
;;; ***** show columns
            (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
            (org-agenda-view-columns-initially t)))
;;; **** CUSTOM AGENDA: Customer Agenda
          ("P2" "Customer Agenda and TEAM PRJ-related tasks" (
                                                              (agenda "")
                                                              (tags "PRJ" ((org-agenda-overriding-header "\nActive Team Projects\n------------------\n")))
                                                              (tags "TEAM" ((org-agenda-overriding-header "\nActive Teams\n------------------\n"))))
           ((org-agenda-files (append '("~/org/02-clients")
                                      (file-expand-wildcards "~/org/02-clients/*")
                                      (file-expand-wildcards "~/org/02-clients/*/*.org")))))
          ("P6" "Agenda and TEAM PRJ-related tasks" (
                                                     (agenda "")
                                                     (tags "PRJ" ((org-agenda-overriding-header "\nActive Team Projects\n------------------\n")))
                                                     (tags "TEAM" ((org-agenda-overriding-header "\nActive Teams\n------------------\n"))))
           ((org-agenda-files (append '("~/org/06-teams")
                                      (file-expand-wildcards "~/org/06-teams/*")
                                      (file-expand-wildcards "~/org/06-teams/*/*.org")))))
          ("Pl" "Refile and stuck" ((tags "REFILE" )
                                    (stuck "")
                                    (todo "TODO"(
                                                 (org-agenda-todo-keyword-format "[ ]")
                                                 (org-agenda-sorting-strategy '(tag-up priority-down))
                                                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n")))))
;;; **** CUSTOM AGENDA: Printed Agenda

          ("Pa" "Printed agenda" ((agenda "" ((org-agenda-ndays 7)
                                              (org-agenda-start-on-weekday nil)
                                              (org-agenda-repeating-timestamp-show-all t)
                                              (org-deadline-warning-days 7)
                                              (org-agenda-todo-keyword-format "[ ]")
                                              (org-agenda-scheduled-leaders '("" ""))
                                        ;(org-agenda-entry-types '(:timestamp :sexp))
                                              ))
                                  (todo "TODO" (
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
;;; Custom Agenda end
          ))
;;; *** ORG CONFIG: Agenda

;;; **** AGENDA Exporter settings
(setq org-agenda-exporter-settings '(
                                     (org-agenda-write-buffer-name "Todays Agenda")
                                     (ps-number-of-columns 2)
                                     (ps-landscape-mode t)
                               ;      (org-agenda-add-entry-text-maxlines 5)
                               ;      (htmlize-output-type 'css)
                                     (ps-print-color-p 'black-white)))


;;; **** AGENDA open in current window
  (setq org-agenda-window-setup (quote current-window))
;;; **** AGENDA warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 2)
;;; **** AGENDA show me tasks scheduled or due in next fortnight
  (setq org-agenda-span (quote fortnight))
;;; **** AGENDA don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;; **** AGENDA global prefix formats
  (setq org-agenda-prefix-format '(


                                   ((agenda . " %i %-12:c%?-12t% s")
                                    (timeline . "  % s")
                                    (todo . " %i %-12:c")
                                    (tags . " %i %-12:c")
                                    (search . " %i %-12:c"))   )
        )

;;; **** AGENDA Tasks mit Datum in der Agenda ausblenden, wenn sie bereits erledigt sind:
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
;;; **** AGENDA don't give awarning colour to tasks with impending deadlines if they are scheduled to be done
  (setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;; **** AGENDA don't show tasks that are scheduled or have deadlines in the normal todo list
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))
;;; **** AGENDA sort tasks in order of when they are due and then by priority
  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-restore-windows-after-quit t)
;;; **** AGENDA start with follow mode?
  ;;(setq org-agenda-start-with-follow-mode t)

;;; **** AGENDA start with sticky?
  (setq org-agenda-sticky t)
;;; **** AGENDA show only the parent tasks in the agenda's lists?
  ;;(setq mby-org-agenda-toggle-list-sublevels nil)

  (setq org-agenda-sorting-strategy
        '((agenda habit-down deadline-up time-up priority-down category-keep)
          (todo category-up todo-state-up priority-down)
          (tags category-up todo-state-up priority-down)
          (search category-keep)))


;;; **** AGENDA enforce todo dependencies
  (setq org-enforce-todo-dependencies 1)

  ;;(setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-dim-blocked-tasks t)
  (setq org-enforce-todo-checkbox-dependencies 1)

  (setq org-columns-default-format
        "%45ITEM %3TODO %5Effort(Time){:} %3PRIORITY %TAGS %6CLOCKSUM(Clock)")
                                        ; Set default column view headings: Task Effort Clock_Summary
                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))

;;; **** AGENDA show all tags available (but only when tagging with : in agenda)
  ;;(setq org-complete-tags-always-offer-all-agenda-tags t)


;;; *** ORG CONFIG: Clocking

  (setq org-agenda-log-mode-items '(state closed clock))

;;; **** CLOCKING auto start tracking default task
  (run-with-idle-timer 25 nil 'bh/clock-in-organization-task-as-default)


;;; **** CLOCKING Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

;;; **** CLOCKING Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
;;; **** CLOCKING Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
;;; **** CLOCKING Change tasks to NEXT when clocking in
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;;; **** CLOCKING Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;;; **** CLOCKING Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
;;; **** CLOCKING remove clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
;;; **** CLOCKING Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
;;; **** CLOCKING Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
;;; **** CLOCKING Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
;;; **** CLOCKING Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;;; **** CLOCKING Include current clocking task in clock reports
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
        ;;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if (and (eq arg 4) tags)
              (org-agenda-clock-in '(16))
            (bh/clock-in-organization-task-as-default)))
      ;;
      ;;; We are not in the agenda
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
  
;;; ** ORG projects (publishing)
;;;; Switches off use of time-stamps when publishing. I would prefer to publish
;;;; everything every time
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

;;; ** ORG Diary
  (setq org-agenda-diary-file "~/org/journal.org")
  
;;; ** ORG Tags
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

;;; ** ORG Tasks
  (setq org-log-redeadline (quote time))
  (setq org-log-done (quote time))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-reschedule (quote time))
;;; *** ORG TODO Keywords
  (setq org-todo-keywords
        (quote (
                (sequence "WISH(w)"  "TODO(t)" "HOLD(w@/!)" "NEXT(n)" "|" "CANCELED(c)" "DONE(d)")
                (sequence "INSERT(i)" "OPEN(o)" "|" "DONE(d)")
                ;;; do we treat ideas as special type of task
                (sequence "RESEARCH(r)" "DESCRIBE(f)" "|" "KNOWLEDGE(k)")
                )))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "pink1" :weight bold)
                ("WISH" :foreground "medium sea green" :weight bold)
                ("INSERT" :foreground "medium sea green" :weight bold)
                ("DONE" :background "darkgrey" :weight bold :box (:line-width 2 :style released-button))
                ("HOLD" :foreground "orange" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))
;;; *** ORG Project tags
  (setq org-tags-exclude-from-inheritance '("PRJ")
        org-stuck-projects '("+PRJ/-HOLD-INSERT-DONE"
                             ("NEXT" "TODO") ("@BUY")))
;;; *** Org mode keyboard remappings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (bind-key "C-c j" 'org-clock-goto) ;;; jump to current task from anywhere
  (bind-key "C-c C-w" 'org-refile)
  ;;; Refile
  (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 7))))
  (setq org-blank-before-new-entry nil)
  :ensure t)
(setq org-list-demote-modify-bullet (quote (("+" . "-")
                                            ("*" . "-")
                                            ("1." . "-")
                                            ("1)" . "a)"))))



;;; * Use Package: Htmlize for exporting agenda
(use-package htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region)
  :ensure t)


(use-package outshine
  :ensure t
  :diminish outline-minor-mode
  :init
  (setq outshine-use-speed-commands t)
  (defvar outline-minor-mode-prefix "\C-o")
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode))
;;; * use Package: Xclip
;;; - for integrating emacs kill-ring with gnome clipboard
(use-package xclip
  :ensure t
  :init (xclip-mode 1))

;;; * Use Package Sh-script
;;; - for editing shell files
(use-package sh-script
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)
         ("\\.zsh" . sh-mode)
         ("\\.bash" . sh-mode)
         )
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))
;;; * Use Package: Markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; * TODO Use Package: habits
(use-package habitica
  :bind (("C-c C-h" . habitica-tasks))
  :ensure t
  )
;;; * Use Package Yaml Mode
(use-package yaml-mode
  :ensure t)
;;; * Use Package: Restclient
(use-package restclient
  :ensure t
  :defer t)
;;; * Use Package: Outshine
;;; - to fold my `init.el' like an org file



(use-package outline
  :defines outline-mode-map
  :commands (outline-mode outline-minor-mode)
  :config
  (bind-keys :map outline-mode-map
             :prefix-map my-outline-prefix
             :prefix "\C-o"
             :menu-name "Outline:Show [a]ll ch[i] [e]ntry [s/k]| Hide [t]body [o]ther [c]entry [l/d] | Move [u/n/p/f/b]"
             :prefix-docstring "Prefix map for convenient folding"
             ("q" . hide-sublevels)    ; Hide everything but the top-level headings
             ("t" . hide-body)         ; Hide everything but headings (all body lines)
             ("o" . hide-other)        ; Hide other branches
             ("c" . hide-entry)        ; Hide this entry's body
             ("l" . hide-leaves)       ; Hide body lines in this entry and sub-entries
             ("d" . hide-subtree)      ; Hide everything in this entry and sub-entries
             ("a" . show-all)          ; Show (expand) everything
             ("e" . show-entry)        ; Show this heading's body
             ("i" . show-children)     ; Show this heading's immediate child sub-headings
             ("k" . show-branches)     ; Show all sub-headings under this heading
             ("s" . show-subtree)      ; Show (expand) everything in this heading & below
             ("u" . outline-up-heading)                ; Up
             ("n" . outline-next-visible-heading)      ; Next
             ("p" . outline-previous-visible-heading)  ; Previous
             ("f" . outline-forward-same-level)        ; Forward - same level
             ("b" . outline-backward-same-level)))       ; Backward - same level



;;; * Use Package: Org Trello
(use-package org-trello
  :mode (("\\.trello$" . org-mode))
  :ensure t)
;;; ** add a hook function to check if this is trello file, then activate the org-trello minor mode.
(add-hook 'org-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name (current-buffer))))
              (when (and filename (string= "trello" (file-name-extension filename)))
                (org-trello-mode)))))
;;; * Use Package: ORG-JIRA
;;; ** credentials are in authinfo
;;; ** you need make sure whether the "/jira" at the end is
;;; ** necessary or not, see discussion at the end of this page
;;; ** jiralib is not explicitly required, since org-jira will load it.
(use-package org-jira
  :config
  (setq jiralib-url "http://acolono.atlassian.net")
  :ensure t)
;;; * Use Package: Gitlab
(unless (package-installed-p 'gitlab)
  (package-install 'gitlab))
;;; * Use Package: Magit
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
;;; * Use Package: org-attach-screenshot
(use-package org-attach-screenshot
  :bind
  (("C-c S" . org-attach-screenshot)))
;;; * Use Package: Web mode
(use-package web-mode
  ;;; org-trello major mode for all .trello files
  :mode (
         ("\\.phtml$" . web-mode)
         ("\\.tpl\\.php" . web-mode)
         ("\\.[agj]sp" . web-mode)
         ("\\.as[cp]x" . web-mode)
         ("\\.erb" . web-mode)
         ("\\.mustache" . web-mode)
         ("\\.djhtml" . web-mode)
         )
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim))
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight t)
  :ensure t)

;;; * Use Package: Org sync
;;; ** use fork of org sync for id in headline
;;; unused because i cannot create issues with this.
(add-to-list 'load-path "~/.emacs.d/org-sync")
(mapc 'load
      '("os" "os-github" "os-bb"))


(add-to-list 'auto-mode-alist '("\\.issues$" . org-mode))

;;(setq org-sync-id-in-headline 1)

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


(defun gh-issue-create ()
  (interactive)
  (gh-issue-new-browse (gh-issue-get-project)
                       (org-get-heading)
                       (org-export-as 'md t)))

(global-set-key (kbd "C-x c g i") 'gh-issue-create)

(server-start)

;;; * org-protocol for capturing from external (i.e. webbrowser)
(require 'org-protocol)

;;; * Print Preview
(setq ps-lpr-command "print_preview")
(setq ps-print-color-p nil)
