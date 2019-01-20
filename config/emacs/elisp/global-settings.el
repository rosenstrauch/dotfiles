
(global-display-line-numbers-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backup settings and autosave ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;


(progn
  ;; make indentation commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1, 24.2, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )

;;; *** STYLE: Display existing tabs as 2 characters wide [#19]
(setq-default tab-width 2)
(setq-default c-basic-offset 2)

