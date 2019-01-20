;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
;; default to 2 visible spaces to display a tab
(setq-default tab-width 2)            ;; but maintain correct appearance
(setq-default tab-width 2)
(setq-default c-basic-offset 2)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

(add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; Configure Backups and autosave

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq auto-save-visited-mode t)
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; revert buffers automatically when underlying files are changed externally

(global-auto-revert-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)


;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(display-battery-mode 1)

;; Increase mode-line size with a border (box) of the same colour and
;; reduce font size by tweaking height
(set-face-attribute 'mode-line nil
		    :inverse-video nil
		    :height 1
		    :box '(:line-width 6 :color "#373b41" :style nil))
(set-face-attribute 'mode-line-inactive nil
		    :inverse-video nil
		    :height 1
		    :box '(:line-width 8 :color "#282a2e" :style nil))

(add-to-list 'default-frame-alist '(width  . 190))
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(font . "Monospace-10"))
;; enable global line numbers
(display-line-numbers-mode)
;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; killring and mark ring
(setq
 kill-ring-max 5000 ; increase kill-ring capacity
 kill-whole-line t  ; if NIL, kill whole line and move the next line up
 )
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )


(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(define-coding-system-alias 'UTF-8 'utf-8)

;; display preferences that go beyond style

(setq org-fontify-whole-heading-line t)
(hl-line-mode)
(global-hl-line-mode)
(visual-line-mode)
(add-to-list 'default-frame-alist
	     '(font . "DejaVu Sans Mono-16"))

(setq auth-sources '((:source "~/.authinfo.gpg")))

(add-hook 'ediff-startup-hook
	  (lambda ()
	    (progn
	      (select-frame-by-name "Ediff")
	      (set-frame-size (selected-frame) 40 10))))
