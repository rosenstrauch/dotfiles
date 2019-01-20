;;; package --- Summary:
;;; Commentary:

;;; Code:
(setq org-agenda-custom-commands
      '(("R" todo "UNREAD")
        ("y" "With type columns" alltodo ""
         ((org-agenda-overriding-columns-format "%20ITEM %TYPE")
          (org-agenda-view-columns-initially t)))
        ;; ...other commands here

        ))

(setq org-agenda-files (list org-directory))


;;; ** ORG CONFIG: Icons [#33]

(setq org-agenda-category-icon-alist
      '(("Emacs" "/usr/share/icons/Moka/16x16/apps/emacs.png" nil nil :ascent center)
        ("experiments" "/usr/share/icons/HighContrast/48x48/emotes/face-monkey.png" nil nil :ascent center)
        ("DROID" "/mnt/DATA/DataLib/PICS/images/icons/Android.png" nil nil :ascent center)
        ("LR" "~/.emacs.d/icons/LR-icon.png" nil nil :ascent center)
        ("AO" "~/.emacs.d/icons/AO_icon.png" nil nil :ascent center)
        ("[Pp]rototypes" "/usr/share/icons/HighContrast/24x24/actions/document-send.png" nil nil :ascent center)
        ("[Ss]pikes" "/usr/share/icons/HighContrast/48x48/emotes/face-raspberry.png" nil nil :ascent center)
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

;;; **** AGENDA Exporter settings [#9]
(setq org-agenda-exporter-settings '(
                                     (org-agenda-write-buffer-name "Todays Agenda")
                                     (ps-number-of-columns 2)
                                     (ps-landscape-mode t)
                                        ;      (org-agenda-add-entry-text-maxlines 5)
                                        ;      (htmlize-output-type 'css)
                                     (ps-print-color-p 'black-white)))

;;; **** AGENDA open in current window [#1]
(setq org-agenda-window-setup (quote current-window))
;;; **** AGENDA warn me of any deadlines in next 7 days [#1]
(setq org-deadline-warning-days 2)
;;; **** AGENDA show me tasks scheduled or due in next fortnight [#1]
(setq org-agenda-span (quote fortnight))
;;; **** AGENDA don't show tasks as scheduled if they are already shown as a deadline [#1]
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;; **** AGENDA global prefix formats [#10]
(setq org-agenda-prefix-format '
      ((agenda . " %i %-12:c%?-12t% s")
       (timeline . "  % s")
       (todo . " %i %-12:c")
       (tags . " %i %-12:c")
       (search . " %i %-12:c")))

;;; **** AGENDA Tasks mit Datum in der Agenda ausblenden, wenn sie bereits erledigt sind: [#2]
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
;;; **** AGENDA don't give awarning colour to tasks with impending deadlines if they are scheduled to be done [#1]
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;; **** AGENDA sort tasks in order of when they are due and then by priority [#2]
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
;;; **** AGENDA start with follow mode? [#2]
;;(setq org-agenda-start-with-follow-mode t)

;;; **** AGENDA start with sticky? [#1]
;;  (setq org-agenda-sticky t)
;;; **** AGENDA show only the parent tasks in the agenda's lists? [#9]
;;(setq mby-org-agenda-toggle-list-sublevels nil)

(setq org-agenda-sorting-strategy
      '((agenda habit-down deadline-up time-up priority-down category-keep)
        (todo category-up todo-state-up priority-down)
        (tags category-up todo-state-up priority-down)
        (search category-keep)))


;;; **** AGENDA enforce todo dependencies [#14]
(setq org-enforce-todo-dependencies 1)

;;(setq org-agenda-dim-blocked-tasks 'invisible)
(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-checkbox-dependencies 1)

;; (setq org-agenda-custom-commands
;;         '(

;;           ("S" . "Status")
;;           ("I" . "Inventory")

;;           ("D" . "Data")
;;           ("M" . "Misc")
;; ;;; **** CUSTOM AGENDA: Review Views [#29]
;;           ("R" . "Review" )
;;           ("Rw" "Week in review"
;;            agenda ""
;;            ((org-agenda-span 'week)
;;             (org-agenda-start-on-weekday 0)
;;             (org-agenda-overriding-header "Week in Review"))
;;            ("/mnt/DATA/exportedata/org-export/review/week.html"))
;;           ("Rd" "Day in review"
;;            agenda ""
;;            ((org-agenda-span 'day)
;;             (org-agenda-overriding-header "Week in Review"))
;;            ("/mnt/DATA/exportedata/org-export/review/day.html"))

;;           ("Rm" "Month in review"
;;            agenda ""
;;            ((org-agenda-span 'month)
;;             (org-agenda-start-day "01")
;;             (org-read-date-prefer-future nil)
;;             (org-agenda-overriding-header "Month in Review"))
;;            ("/mnt/DATA/exportedata/org-export/review/month.html"))

;; ;;; **** CUSTOM AGENDA: Priority views [#24]
;;           ("c" . "Priority views")
;;           ("ca" "#A"  ((tags "PRIORITY=\"A\""
;;                              ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                               (org-agenda-overriding-header "High-priority unfinished tasks:")))
;;                        (alltodo ""
;;                                 ((org-agenda-entry-types '(:scheduled))
;;                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                                       "\\[#A\\]"))))))
;;           ("cb" "#B" alltodo ""
;;            ((org-agenda-entry-types '(:scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                  "\\[#B\\]"))))
;;           ("cc" "#C" alltodo ""
;;            ((org-agenda-entry-types '(:scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                  "\\[#C\\]"))))
;;           ("h" "Habits" tags-todo "STYLE=\"habit\""
;;            ((org-agenda-overriding-header "Habits")
;;             (org-agenda-sorting-strategy
;;              '(todo-state-down effort-up category-keep))))

;;           ("S" . "Status")
;;           ("I" . "Inventory")
;;           ("Q" . "Quests")
;;           ("D" . "Data")
;;           ("M" . "Misc")
;; ;;; **** CUSTOM AGENDA: Review Views [#29]
;;           ("R" . "Review" )
;;           ("Rw" "Week in review"
;;            agenda ""
;;            ((org-agenda-span 'week)
;;             (org-agenda-start-on-weekday 0)
;;             (org-agenda-overriding-header "Week in Review"))
;;            ("/mnt/DATA/exportedata/org-export/review/week.html")
;;            )


;;           ("Rd" "Day in review"
;;            agenda ""
;;            ((org-agenda-span 'day)
;;             (org-agenda-overriding-header "Week in Review"))

;;            ("/mnt/DATA/exportedata/org-export/review/day.html")
;;            )

;;           ("Rm" "Month in review"
;;            agenda ""
;;            ((org-agenda-span 'month)
;;             (org-agenda-start-day "01")
;;             (org-read-date-prefer-future nil)
;;             (org-agenda-overriding-header "Month in Review"))

;;            ("/mnt/DATA/exportedata/org-export/review/month.html")
;;            )

;; ;;; **** CUSTOM AGENDA: Priority views [#24]
;;           ("c" . "Priority views")
;;           ("ca" "#A"  (
;;                        (tags "PRIORITY=\"A\""
;;                              ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
;;                               (org-agenda-overriding-header "High-priority unfinished tasks:")))
;;                        (alltodo ""
;;                                 ((org-agenda-entry-types '(:scheduled))
;;                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                                       "\\[#A\\]"))))))
;;           ("cb" "#B" alltodo ""
;;            ((org-agenda-entry-types '(:scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                  "\\[#B\\]"))))
;;           ("cc" "#C" alltodo ""
;;            ((org-agenda-entry-types '(:scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp
;;                                                                  "\\[#C\\]"))))
;;           ("h" "Habits" tags-todo "STYLE=\"habit\""
;;            ((org-agenda-overriding-header "Habits")
;;             (org-agenda-sorting-strategy
;;              '(todo-state-down effort-up category-keep))))

;; ;;; **** CUSTOM AGENDA: Quests [#1]
;;           ("Q" . "Quests") ;;; gives label to "Q"
;;           ("Qo" "Todos tagged as Issues" tags-todo "-SCHEDULED={.+}/!+ISSUE")
;; ;;; ***** quests dont contain issues and boards
;; ;;; ***** quests that are estimated, are not scheduled, are not DONE.
;; ;;; ***** quests to estimate next [#1]
;;           ("Qi" "[i]nserted unestimated tasks" tags-todo "Effort<1-SCHEDULED={.+}/!-DONE" )
;; ;;; ***** quests which have estimates [#1]
;;           ("Qs" "[s]chedule next tasks" tags-todo "Effort>1-SCHEDULED={.+}/!-DONE")
;; ;;; ***** quests scheduled for today [#10]
;;           ("Qt" "Do TODAY" agenda ""
;;            ((org-agenda-ndays 1)
;;             (org-agenda-use-time-grid nil)
;;             (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
;;             (org-agenda-view-columns-initially t)))
;;           ("Qd" "Upcoming deadlines" agenda ""
;;            ((org-agenda-entry-types '(:deadline))
;;             (org-agenda-ndays 1)
;;             (org-deadline-warning-days 60)
;;             (org-agenda-time-grid nil)))
;; ;;; **** CUSTOM AGENDA: queries/searches [#1]
;;           ("F" . "Find - Custom queries/searches") ;;; gives label to "Q"
;; ;;; ***** sparse tree for next string [#5]
;;           ("Fn" "Next in file" occur-tree "NEXT")
;;           ("Fi" "Issue search" search ""
;;            ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
;;           ("FA" "Archive search" search ""
;;            ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
;; ;;; ***** match tagged headlines that are not todos [#3]
;;           ("K" "Knowledge" tags "+{.*}+TODO=\"\"|+{.*}+TODO=\"DONE\""
;;            ((org-tags-match-list-sublevels 'indented)
;;             (org-agenda-sorting-strategy '(tag-up))))
;; ;;; ***** match orphan headlines (the ones without tag or todo) [#6]
;;           ("O" "Orphans" tags "-{.*}+TODO=\"\""
;;            ((org-tags-match-list-sublevels 'indented)))
;;           ("P" . "Projects") ;;; gives label to "P"
;;           ("P1" "Internal Block"
;;            ((tags "PRJ" ((org-agenda-overriding-header "\nInternal Projects\n------------------\n")))
;;             (tags-todo "Effort>1-SCHEDULED={.+}/!-DONE" ((org-agenda-overriding-header "\Estimated Unscheduled Internal Tasks\n------------------\n"))))
;; ;;; ***** only look in internal org files [#1]
;;            ((org-agenda-files (file-expand-wildcards "~/org/01-internal/*/*"))
;; ;;; ***** show columns [#2]
;;             (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
;;             (org-agenda-view-columns-initially t)))
;; ;;; **** CUSTOM AGENDA: Customer Agenda [#20]
;;           ("P2" "Customer Agenda and TEAM PRJ-related tasks" (
;;                                                               (agenda "")
;;                                                               (tags "PRJ" ((org-agenda-overriding-header "\nActive Team Projects\n------------------\n")))
;;                                                               (tags "TEAM" ((org-agenda-overriding-header "\nActive Teams\n------------------\n"))))
;;            ((org-agenda-files (append '("~/org/02-clients")
;;                                       (file-expand-wildcards "~/org/02-clients/*")
;;                                       (file-expand-wildcards "~/org/02-clients/*/*.org")))))
;;           ("P6" "Agenda and TEAM PRJ-related tasks" (
;;                                                      (agenda "")
;;                                                      (tags "PRJ" ((org-agenda-overriding-header "\nActive Team Projects\n------------------\n")))
;;                                                      (tags "TEAM" ((org-agenda-overriding-header "\nActive Teams\n------------------\n"))))
;;            ((org-agenda-files (append '("~/org/06-teams")
;;                                       (file-expand-wildcards "~/org/06-teams/*")
;;                                       (file-expand-wildcards "~/org/06-teams/*/*.org")))))
;;           ("Pl" "Refile and stuck" ((tags "REFILE" )
;;                                     (stuck "")
;;                                     (todo "TODO"(
;;                                                  (org-agenda-todo-keyword-format "[ ]")
;;                                                  (org-agenda-sorting-strategy '(tag-up priority-down))
;;                                                  (org-agenda-overriding-header "\nTasks by Context\n------------------\n")))))
;; ;;; **** CUSTOM AGENDA: Printed Agenda [#22]

;;           ("Pa" "Printed agenda" ((agenda "" ((org-agenda-ndays 7)
;;                                               (org-agenda-start-on-weekday nil)
;;                                               (org-agenda-repeating-timestamp-show-all t)
;;                                               (org-deadline-warning-days 7)
;;                                               (org-agenda-todo-keyword-format "[ ]")
;;                                               (org-agenda-scheduled-leaders '("" ""))
;;                                         ;(org-agenda-entry-types '(:timestamp :sexp))
;;                                               ))
;;                                   (todo "TODO" (
;;                                                 (org-agenda-todo-keyword-format "")
;;                                         ;(org-agenda-prefix-format  "%i %-12:c %(concat \"[ \"(org-format-outline-path (org-get-outline-path)) \" ]\") ")
;;                                                 (org-agenda-sorting-strategy '(category-up priority-down))
;;                                                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
;;            ((org-agenda-with-colors nil)
;;             (org-agenda-compact-blocks t)
;;             ;;(org-agenda-remove-tags t)
;;             (ps-number-of-columns 1)
;;             (org-tags-match-list-sublevels 'indented)
;;             (ps-landscape-mode nil))

;;            ("/mnt/DATA/exportedata/org-export/agenda.pdf"))
;; ;;; Custom Agenda end [#1]
;;           )
