;;; - CLOCKING Separate drawers for clocking and logs [#1]
;;; - CLOCKING Save clock data and state changes and notes in the LOGBOOK drawer [#1]
;;; - CLOCKING remove clocked tasks with 0:00 duration [#1]
;;; - CLOCKING Clock out when moving task to a done state [#1]
;;; - CLOCKING Save the running clock and all clock history when exiting Emacs, load it on startup [#1]
;;; - CLOCKING Do not prompt to resume an active clock [#1]
;;; - CLOCKING Enable auto clock resolution for finding open clocks [#1]
;;; - CLOCKING Show lot of clocking history so it's easy to pick items off the C-F11 list [#1]


  (setq-default org-clock-history-length 23
                org-clock-in-resume t
                org-clock-into-drawer t
                org-clock-out-remove-zero-time-clocks t
                org-clock-out-when-done t
                org-clock-persist 'history
                org-clock-persist-query-resume nil
                org-log-done 'time
                org-timer-default-timer 25
                org-drawers (quote ("PROPERTIES" "LOGBOOK"))
                org-clock-auto-clock-resolution (quote when-no-clock-is-running)
                )
;;; **** CLOCKING Resume clocking task on clock-in if the clock is open [#1]

  (setq org-agenda-log-mode-items '(state closed clock))

;;; **** CLOCKING auto start tracking default task [#3]
  (run-with-idle-timer 25 nil 'bh/clock-in-organization-task-as-default)


;;; **** CLOCKING Resume clocking task when emacs is restarted [#2]
  (org-clock-persistence-insinuate)

;;; **** CLOCKING Change tasks to NEXT when clocking in [#1]
  (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

;;; **** CLOCKING Include current clocking task in clock reports [#128]
  (setq org-clock-report-include-clocking-task t)

  (setq bh/keep-clock-running nil)

