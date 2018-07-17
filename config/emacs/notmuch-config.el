;;; notmuch-config.el --- my notmuch settings"

;;; Commentary:


;;; Code:
(setq notmuch-archive-tags '("-unread" "+archive" "-inbox"))
(setq notmuch-saved-searches '(
                               (:name "notifications" :query "tag:notifications")
                               (:name "Y2018" :query "date:2017..2018")t
			                         (:name "unread" :query "tag:unread" :key "u")
			                         (:name "flagged" :query "tag:flagged" :key "f")
			                         (:name "sent" :query "tag:sent" :key "t")
			                         (:name "drafts" :query "tag:draft" :key "d")
			                         (:name "all mail" :query "*" :key "a")
                               (:name "inbox" :query "tag:inbox" :count-query "tag:inbox and tag:unread" :sort-order oldest-first :key "i")))

(provide 'notmuch-config)
;;; notmuch-config.el ends here
