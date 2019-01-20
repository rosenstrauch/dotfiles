;;; **** ORG MOBILE: configuration. [#2]
  (setq org-mobile-directory "/ssh:rosen_sync@files.rosenstrauch.com:/home/rosen_sync/roSynxcBox/MobileOrg/")

;;; **** ORG MOBILE: the name of the file where new notes will be stored [#1]
  (setq org-mobile-inbox-for-pull (concat org-directory "/flagged.org"))


  ;; Automatically sync mobileorg when idle
  (defvar org-mobile-sync-timer nil)
  (defvar org-mobile-sync-idle-secs (* 60 10))
  (defun org-mobile-sync ()
    (interactive)
    (org-mobile-pull)
    (org-mobile-push))
  (defun org-mobile-sync-enable ()
    "enable mobile org idle sync"
    (interactive)
    (setq org-mobile-sync-timer
	  (run-with-idle-timer org-mobile-sync-idle-secs t
			       'org-mobile-sync)))
  (defun org-mobile-sync-disable ()
    "disable mobile org idle sync"
    (interactive)
    (cancel-timer org-mobile-sync-timer))
  (org-mobile-sync-enable)
