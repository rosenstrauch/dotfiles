;;; package --- Summary

;;; Commentary:

;;; Code:

(setq erc-modules '(
  autojoin
  button
  completion
  fill
  irccontrols
  list
  log
  match
  menu
  move-to-prompt
  netsplit
  networks
  noncommands
  notifications
	notify
  readonly
  ring
  stamp
  track))

(erc-update-modules)

(provide 'ercrc)
;;; ercrc.el ends here
