(setq org-capture-templates
      '(("c" "Catch all")
        ("L" "capture website through org protocol" entry
         (file+olp+datetree "" "REFILE")
         (file "~/.emacs.d/my-org-capture-templates/capture-link.org"))
        ("p" "capture text from website through org protocol" entry
         (file+olp+datetree "" "REFILE")

         (file "~/.emacs.d/my-org-capture-templates/capture-link.org"))
        ("cc" "Capture Entry" entry
         (file+olp+datetree "" "REFILE")
         (file "~/.emacs.d/my-org-capture-templates/capture-entry.org") :prepend t)))

;;   (setq org-capture-templates
;;         '(("s" "ScreenShot" entry (file+headline "~/org/capture.org" "ScreenShots")
;;            "* %?\n  %i\n  %a")
;;           ("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
;;            "* WISH %?\n  %i\n  %a")
;;           ("f" "Financial entries")
;;           ("fm" "bankcard" plain
;;            (file "~/org/07-needs/finance.ledger")
;;            "%(org-read-date) %^{Payee}
;;   Liabilities:bankcard
;;   Expenses:%^{Account}  %^{Amount}
;; ")
;;           ("fc" "Cash" plain
;;            (file "~/org/07-needs/finance.ledger")
;;            "%(org-read-date) * %^{Payee}
;;   Expenses:Cash
;;   Expenses:%^{Account}  %^{Amount}
;; ")
;; ;;; **** ORG CONFIG Capture contacts
;;           ("c" "👤 Contact" entry
;;            (file+headline "~/contacts.org" "People")
;;            "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:"
;;            :clock-keep t :kill-buffer t)

;;           ("O" "Company Contacts" entry
;;            (file+headline (expand-file-name "contacts.org" org-directory) "Companies")
;;            "* %(org-contacts-template-name)
;;  :PROPERTIES:
;;  :EMAIL: %(org-contacts-template-email)
;;  :PHONE:
;;  :ALIAS:
;;  :NICKNAME:
;;  :IGNORE:
;;  :ICON:
;;  :NOTE:
;;  :ADDRESS:
;;  :BIRTHDAY:
;;  :END:")

;; ;;; **** CAPTURE: Ideas are not tasks                                            [#2]
;;           ("i" "Idea" entry (file+headline "~/org/ideas.org" "IdeaInbox")
;;            "* %?\nEntered on %U\n  %i\n  %a")
;; ;;; **** CAPTURE: journal entries do not show up in agenda (but maybe as diary) [#2]
;;           ("j" "Journal" entry (file+datetree "~/org/journal.org")
;;            "* %?\nEntered on %U\n  %i\n  %a")
;; ;;; **** CAPTURE: calendar entries prompt for date and show up in agenda may or maynot be todos [#2]
;;           ("D" "Date" entry (file+datetree "~/org/calendar.org")
;;            "* %?\nEntered on %^T\n  %i\n  %a")
;; ;;; **** CAPTURE: Routines are repeated tasks captured to routines.org [#2]
;;           ("r" "Routine" entry (file "~/org/routines.org")
;;            "** TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
;; ;;; **** CAPTURE: LinkLibrary is sorted by capture date in a date tree [#2]
;;           ("l" "Link" entry (file+olp+datetree "~/org/links.org")
;;            "* %c %a %x %?\nEntered on %U\n  %i\n" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)
;; ;;; **** CAPTURE: Snippets will need to get refiled if i capture a link it may include a snipptet [#6]
;;           ("x" "Firefox Capture Template" entry
;;            (file+headline "~/org/capture.org" "ClippedSnippets")
;;            "* Snippets %a\n%i\nEntered on %U\n%c\ \nNote: %?\nLink: %l" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)))
