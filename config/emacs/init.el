;;; package --- Summary
;;; Commentary:
;;; Code:
;;; * GLOBAL Emacs Settings
;;; ** GLOBAL Load custom Lisp from others [#1]


(require 'server)
(unless (server-running-p)
  (server-start))

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; ** GLOBAL Store Customizations elsewhere [#6]
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (with-temp-buffer (write-file custom-file)))
(load custom-file)

;;; ** GLOBAL Configure Backups [#5]
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
;;; ** GLOBAL KEYMAPs
;;; *** Match parenthesis with % [#9]

(global-set-key "%" 'match-paren)

;;; *** KEYMAP Next and previous page [#2]
(define-key prog-mode-map "\C-x\C-n" #'forward-page)
(define-key prog-mode-map "\C-x\C-p" #'backward-page)
;;; *** KEYMAP Duplicate line C-d [#10]

(global-set-key (kbd "C-d") 'duplicate-line)

;;; *** KEYMAP quick access to init file C-c I

(global-set-key (kbd "C-c I") 'find-user-init-file)
;;; *** KEYMAP TODO quick access to home.org
;;; ** STYLE

;;; *** STYLE: Autorecognize Page Breaks [#6]
(setq page-delimiter
      (rx bol (or "\f" ";;;")
          (not (any "#")) (* not-newline) "\n"
          (* (* blank) (opt ";" (* not-newline)) "\n")))
;; Expanded regexp:
;; "^;;;[^#].*\n\\(?:[[:blank:]]*\\(?:;.*\\)?\n\\)*"
;;; *** STYLE: highlight Matching parenthesis
(show-paren-mode 1)
;;; *** STYLE: Make tabs into spaces when you type them
;;(setq-default indent-tabs-mode nil)

;;; *** STYLE: Display existing tabs as 2 characters wide [#19]
(setq-default tab-width 2)
(setq-default c-basic-offset 2)


(load-theme 'tsdh-dark)

(set-frame-font "9x15")
(global-display-line-numbers-mode t)
(column-number-mode 1)
(setq line-spacing '0.25)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "white"))))
 '(org-level-1 ((t (:inherit outline-1 :background "blue" :foreground "lavender blush" :box nil :height 1.4))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "orange" :box nil :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "magenta" :box nil :height 0.9))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "khaki" :box nil :height 0.7))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "orchid1" :box nil :height 0.5)))))

;;; ** CUSTOM functions
;;; *** CUSTOM open emacs config
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;; *** CUSTOM: duplicate line
(defun duplicate-line()
  "Duplicates a line."
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (forward-line 1)
  (yank)
  )
;;; *** CUSTOM match parenthesis

(defun match-paren (arg)
  "Interactively Go to the matching paren if ARG on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))


;;; * BOOTSTRAP Package Management
;;; ** BOOTSTRAP package.el [#5]
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)
                                        ;(package-refresh-contents)
                                        ;                                        (byte-recompile-directory package-user-dir nil 'force)
;;; ** BOOTSTRAP `use-package' [#1]
(unless (package-installed-p 'use-package)
;;; TODO: dont run this on every start but dont not run it so we dont have outdated lists. [#4]
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;;; * Packages

;;; - use-package is available from here on [#1]


;;; * Print Preview [#2]
(setq ps-lpr-command "print_preview")
(setq ps-print-color-p nil)

;;; * Notmuch
(autoload 'notmuch "notmuch" "notmuch mail" t)


;;; * Use package: babel restclient
(use-package ob-restclient
  :ensure t
  :defer t)

;;; * use package: org-screenshot
(use-package org-attach-screenshot
  :ensure t)

;;; * Use package: emacs-fasd
(use-package fasd
  :bind(("\C-c f" . fasd-find-file))
  :config (setq global-fasd-mode 1)
  (setq fasd-enable-initial-prompt nil)
  :ensure t)

;;; * Use package: csv-export
(use-package org-clock-csv
  :ensure t)

;;; * Use Package: Htmlize for exporting agenda [#8]
(use-package htmlize
  :commands (htmlize-buffer
             htmlize-file
             htmlize-many-files
             htmlize-many-files-dired
             htmlize-region)
  :ensure t)


;;; * use Package: Xclip
;;; - for integrating emacs kill-ring with gnome clipboard [#4]
(use-package xclip
  :ensure t
  :init (xclip-mode 1))

;;; * TODO Use Package: habits [#4]
(use-package habitica
  :bind (("C-c C-h" . habitica-tasks))
  :ensure t)

;;; * Use Package: org-attach-screenshot [#3]
(use-package org-attach-screenshot
  :bind
  (("C-c S" . org-attach-screenshot)))

;;; * Use Package: emacs-w3m
(use-package w3m
  :config
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

;;; * Use package org-invoice
;;; https://github.com/jbranso/.emacs.d/blob/master/lisp/init-org.org#org-invoice
  (use-package org-invoice )

;;; * use package: helm
  (use-package helm
    :bind (("M-x" . helm-M-x)
           ("M-<f5>" . helm-find-files)
           ([f10] . helm-buffers-list)
           ([S-f10] . helm-recentf))
    :ensure t)


;;; * Use package tramp

  (use-package tramp
    :config
    (setq tramp-default-method "ssh")
    (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
    (setq tramp-chunksize 500)
    (setq tramp-default-method "ssh")
    (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash")
                     ))

;;; * Use Package: ORG-JIRA
;;; ** credentials are in authinfo, you need make sure whether the "/jira" at the end is necessary or not jiralib is not explicitly required, since org-jira will load it. [#4]
                                        ;(use-package org-jira
                                        ;  :config
                                        ;  (setq jiralib-url "http://acolono.atlassian.net")
                                        ;  :ensure t)
;;; * Package: Gitlab [#2]
  (unless (package-installed-p 'gitlab)
    (package-install 'gitlab))

;;; * Use Package: Magit [#11]
  (use-package magit
    :bind ("C-x g" . magit-status)
    :config
    (progn
      (setenv "GIT_PAGER" "")
      (add-hook 'magit-log-edit-mode-hook
                '(lambda ()
                   (auto-fill-mode)
                   (flyspell-mode)
                   (set-fill-column 80)))))
  ;; optional keyboard short-cut
  (global-set-key "\C-xm" 'browse-url-at-point)
  :ensure t)

(use-package ghub
	:ensure t)
;;; * Use package: git timemachine

(use-package git-timemachine
  :ensure t
  :commands (git-timemachine-toggle
             git-timemachine-switch-branch)
  :bind* (("M-m g l" . git-timemachine-toggle)
          ("M-m g L" . git-timemachine-switch-branch)))

(provide 'init-git-timemachine)


;;; * Use Package js2-mode [#2]
(use-package js2-mode
	:mode
  (("\\.js$" . js2-mode)
	 ("\\.jsx$" . js2-jsx-mode))
  :ensure t
	:hook ((js2-mode . (lambda ()
                       (flycheck-mode)
))
         (js2-jsx-mode . (lambda ()
                           (flycheck-mode)
)))
	:config
	;; have 2 space indentation by default
  (setq js-indent-level 2
        js2-basic-offset 2
				js-chain-indent t)
	)
;; indium: javascript awesome development environment
;; https://github.com/NicolasPetton/indium
(use-package indium
	:ensure t
  :after js2-mode
  :bind (:map js2-mode-map

              ("C-c C-l" . indium-eval-buffer)
              ("C-c C-n" . indium-run-node)
              ("C-c b l" . indium-list-breakpoints)
              ("C-c b K" . indium-remove-all-breakpoints-from-buffer)
              ("C-c b t" . indium-toggle-breakpoint)
							)
  :hook ((js2-mode . indium-interaction-mode)))


(use-package js-auto-beautify
  :ensure t)

(use-package js-import
  :ensure t)

;;; * Use Package Elm Mode [#2]

(use-package elm-mode
  :ensure t)

;;; * Use Package Yaml Mode [#2]
(use-package yaml-mode
  :ensure t)

;;; * Use Package Json Mode [#2]
(use-package json-mode
  :ensure t)

;;; * Use Package Makefile ob [#2]
(use-package ob-makefile
  :config
  (setq org-src-preserve-indentation t))

;;; * Use Package Make Mode [#2]
(use-package make-mode
  :ensure t)
;;; * Use Package: Restclient [#3]
(use-package restclient
  :ensure t
  :defer t)

;;; * use package org-contacts
;;; - https://emacs.stackexchange.com/a/17553
(use-package org-contacts
  :config
  (setq org-contacts-files `(,(expand-file-name "contacts.org" org-directory)))
  (setq org-contacts-icon-use-gravatar nil)
  (setq org-contacts-address-property "CITY")
  :ensure org-plus-contrib)

;;; * Use Package Sh-script
;;; - for editing shell files [#9]
(use-package sh-script
  :mode (("\\.*bashrc$" . sh-mode)
         ("\\.*bash_profile" . sh-mode)
         ("\\.zsh" . sh-mode)
         ("\\.bash" . sh-mode)
         )
  :config
  (setq-default sh-indentation 2
                sh-basic-offset 2))

;;; * Use Package: Markdown mode [#8]
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; * Use Package dockerfile-mode [#2]
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


;;; * Use package flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
	:config
	;; use local eslint from node_modules before global
	;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
	(defun my/use-eslint-from-node-modules ()
		(let* ((root (locate-dominating-file
									(or (buffer-file-name) default-directory)
									"node_modules"))
					 (eslint (and root
												(expand-file-name "node_modules/eslint/bin/eslint.js"
																					root))))
			(when (and eslint (file-executable-p eslint))
				(setq-local flycheck-javascript-eslint-executable eslint))))
	(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  )
;;; * Use Package php Mode [#2]
(use-package php-mode
  :ensure t)

(use-package phpcbf
  :ensure t
  :init
  (setq phpcbf-executable "~/bin/phpcbf")
  :config
  (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
  (setq phpcbf-standard "Drupal")
  )

;; (use-package flymake-php
;;   :ensure t
;;   :config
;;   (add-hook 'php-mode-hook 'flymake-php-load))

;; (use-package flymake-phpcs
;;   :ensure t
;;   :init
;;    (setq flymake-phpcs-standard "Drupal")
;;   :config


;; ;; Customize the coding standard checked by phpcs
;;   (setq flymake-phpcs-standard
;;         "~/.fresh/build/src/PHP_CodeSniffer/CodeSniffer/Standards/Drupal")

;; ;; Show the name of sniffs in warnings (eg show
;; ;; "Generic.CodeAnalysis.VariableAnalysis.UnusedVariable" in an unused
;; ;; variable warning)
;; (setq flymake-phpcs-show-rule t))

;;; * Use Package: Org Trello [#3]
(use-package org-trello
  :mode "\\.trello$"
  :interpreter "org-mode"
  :ensure t)

;;; * Use Package: Web mode [#20]
(use-package web-mode
  :mode (
         ("\\.phtml$" . web-mode)
         ("\\.tpl\\.php" . web-mode)
         ("\\.[agj]sp" . web-mode)
         ("\\.as[cp]x" . web-mode)
         ("\\.erb" . web-mode)
;         ("\\.js" . web-mode)
         ("\\.mustache" . web-mode)
         ("\\.djhtml" . web-mode)
         )
  :bind
  (:map web-mode-map
        ("," . self-with-space)
        ("<C-return>" . html-newline-dwim))
  :config
  (setq web-mode-enable-auto-quoting nil
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2)
  :ensure t)

;;(setq org-sync-id-in-headline 1)

;;; * Use package: ledger-mode

(use-package ledger-mode
  :ensure t
  :config
  (defun ledger-add-entry (title in amount out)
    (interactive
     (let ((accounts (mapcar 'list (ledger-accounts))))
       (list (read-string "Entry: " (format-time-string "%Y-%m-%d " (current-time)))
             (let ((completion-regexp-list "^Ausgaben:"))
               (completing-read "What did you pay for? " accounts))
             (read-string "How much did you pay? " "EUR ")
             (let ((completion-regexp-list "^Vermögen:"))
               (completing-read "Where did the money come from? " accounts)))))
    (insert title)
    (newline)
    (indent-to 4)
    (insert in "  " amount)
    (newline)
    (indent-to 4)
    (insert out))
  :mode "\\.ledger$"
  :commands ledger-mode)


;;; * Use Package: Outshine
;;; :PROPERTIES:
;;;  :CUSTOM_ID: use_outshine.init.el
;;;  :END:
;;;

(use-package outshine
  :ensure t
  :diminish outline-minor-mode
  :init
  (defvar outline-minor-mode-prefix "C-o")
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  :config
  (setq outshine-use-speed-commands t))


;;; * Use Package: Outshine
;;; - to fold my `init.el' like an org file [#31]

(use-package outline
  :ensure t
  :defines outline-mode-map
  :commands (outline-mode outline-minor-mode)
  :config
  (bind-keys :map outline-mode-map
             :prefix-map my-outline-prefix
             :prefix "\C-o"
             :menu-name "Outline:Show [a]ll ch[i] [e]ntry [s/k]| Hide [t]body [o]ther [c]entry [l/d] | Move [u/n/p/f/b]"
             :prefix-docstring "Prefix map for convenient folding"
             ("q" . outshine-hide-sublevels)    ; Hide everything but the top-level headings
             ("t" . outline-hide-body)         ; Hide everything but headings (all body lines)
             ("o" . outline-hide-other)        ; Hide other branches
             ("c" . outline-hide-entry)        ; Hide this entry's body
             ("l" . outline-hide-leaves)       ; Hide body lines in this entry and sub-entries
             ("d" . outline-hide-subtree)      ; Hide everything in this entry and sub-entries
             ("a" . outline-show-all)          ; Show (expand) everything
             ("e" . outline-show-entry)        ; Show this heading's body
             ("i" . outline-show-children)     ; Show this heading's immediate child sub-headings
             ("k" . outline-show-branches)     ; Show all sub-headings under this heading
             ("s" . outline-show-subtree)      ; Show (expand) everything in this heading & below
             ("u" . outline-up-heading)                ; Up
             ("n" . outline-next-visible-heading)      ; Next
             ("p" . outline-previous-visible-heading)  ; Previous
             ("f" . outline-forward-same-level)        ; Forward - same level
             ("b" . outline-backward-same-level)))       ; Backward - same level


;;; * use package mocha

(use-package mocha
	:bind(
				("C-c m P" . mocha-test-project)
				("C-c m f" . mocha-test-file)
				("C-c m p" . mocha-test-at-point))
	:ensure t)
;;; * Use Package: ORG MODE [#4]
(use-package org
  :mode ("\\.jl\\'" . org-mode)
  :load-path ("rosenorg-lisp/")
  :ensure t

;;; ** ORG KEYBINDINGS
;;; C-c ^ | (org-sort)
;;; C-c c | capture
;;; C-c +  | toggle sublevels (needs reload) [#4]
;;;  C-c k | Cut Subtree
  :bind (("C-c r" . org-sort)
         ("C-c c" . org-capture)
         ("C-c +" . mby-org-agenda-toggle-list-sublevels)
         ("C-k" . org-cut-subtree)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c s b" . org-iswitchb)
         ("C-c j" . org-clock-goto) ;;; jump to current task from anywhere
         ("C-c C-w" . org-refile)
         ("C-c d" . org-refile-to-datetree)
         ("C-c i s" . my-org-screenshot)
         ("C-c o c" . org-contacts)
         ("C-<" . org-begin-template))
;;; ** ORG INIT [#1]
  :init
;;; *** ORG INIT github issue new url
  (defun gh-issue-new-url (project title body)
    "Create url for new issue.  PROJECT is the repo.  TITLE is the name of the issue.  BODY describes the issue."
    (concat "https://github.com/"
            project
            "/issues/new?title="
            (url-hexify-string title)
            "&body="
            (url-hexify-string body)))

  (defun gh-issue-new-browse (project title body)
    "Browse to url of new issue.  PROJECT is the repo.  TITLE is the name of the issue.  BODY describes the issue."
    (browse-url (gh-issue-new-url project title body)))

  (defun gh-issue-get-project ()
    "Read project from property."
    (org-entry-get (point) "GH-PROJECT" t))


  (defun gh-issue-create ()
    "Interactively create a new issue."
    (interactive)
    (gh-issue-new-browse (gh-issue-get-project)
                         (org-get-heading)
                         (org-export-as 'md t)))

  (global-set-key (kbd "C-x c g i") 'gh-issue-create)

;;; *** ORG INIT Clocking

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

  (defvar bh/organization-task-id "d4dcfc17-e1c5-462c-9c9c-b92013e92915")

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

;;; *** ORG INIT date insert function https://www.emacswiki.org/emacs/InsertAnyDate
  (require 'calendar)

  (defun insdate-insert-any-date (date)
    "Insert DATE using the current locale."
    (interactive (list (calendar-read-date)))
    (insert (calendar-date-string date)))

  (defun insdate-insert-date-from (&optional days)
    "Insert date that is DAYS from current."
    (interactive "p*")
    (insert
     (calendar-date-string
      (calendar-gregorian-from-absolute
       (+ (calendar-absolute-from-gregorian (calendar-current-date))
          days)))))

  (add-hook 'before-save-hook #'endless/update-includes)

  (defun endless/update-includes (&rest ignore)
    "Update the line numbers of #+INCLUDE:s in current buffer.
Only looks at INCLUDEs that have either :range-begin or :range-end.
This function does nothing if not in org-mode, so you can safely
add it to `before-save-hook'."
    (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
                nil 'noerror)
          (let* ((file (expand-file-name (match-string-no-properties 1)))
                 lines begin end)
            (forward-line 0)
            (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
              (setq begin (match-string-no-properties 1)))
            (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
              (setq end (match-string-no-properties 1)))
            (setq lines (endless/decide-line-range file begin end))
            (when lines
              (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
                  (replace-match lines :fixedcase :literal nil 1)
                (goto-char (line-end-position))
                (insert " :lines \"" lines "\""))))))))

;;; *** ORG INIT: provide command to fix includes
  (defun endless/decide-line-range (file begin end)
    "Visit FILE and decide which lines to include.
BEGIN and END are regexps which define the line range to use."
    (let (l r)
      (save-match-data
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (if (null begin)
              (setq l "")
            (search-forward-regexp begin)
            (setq l (line-number-at-pos (match-beginning 0))))
          (if (null end)
              (setq r "")
            (search-forward-regexp end)
            (setq r (1+ (line-number-at-pos (match-end 0)))))
          (format "%s-%s" l r)))))
;;; *** ORG INIT: provide a command to show the subtasks
;;; *** ORG INIT: NOTE: org-agenda-dim-blocked-tasks may not be invisible [#7]

  (defun mby-org-agenda-toggle-list-sublevels ()
    "Toggle the display of the subtasks in the agenda lists. between nil and t"
    (message "agenda-toggle-list-sublevels")

    (interactive)
    (setq org-agenda-todo-list-sublevels (not org-agenda-todo-list-sublevels)))



;;; *** ORG INIT: Limit amount of tasks in NEXT state per project [#27]

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

;;; *** ORG INIT function to wrap blocks of text in org templates
  (defun org-begin-template ()
    "Make a template at point."
    (if (org-at-table-p)
        (call-interactively 'org-table-rotate-recalc-marks)
      (let* ((choices '(("s" . "SRC")
                        ("e" . "EXAMPLE")
                        ("q" . "QUOTE")
                        ("v" . "VERSE")
                        ("c" . "CENTER")
                        ("l" . "LaTeX")
                        ("h" . "HTML")
                        ("a" . "ASCII")))
             (key
              (key-description
               (vector
                (read-key
                 (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                         (mapconcat (lambda (choice)
                                      (concat (propertize (car choice) 'face 'font-lock-type-face)
                                              ": "
                                              (cdr choice)))
                                    choices
                                    ", ")))))))
        (let ((result (assoc key choices)))
          (when result
            (let ((choice (cdr result)))
              (cond
               ((region-active-p)
                (let ((start (region-beginning))
                      (end (region-end)))
                  (goto-char end)
                  (insert "#+END_" choice "\n")
                  (goto-char start)
                  (insert "#+BEGIN_" choice "\n")))
               (t
                (insert "#+BEGIN_" choice "\n")
                (save-excursion (insert "#+END_" choice))))))))))



;;; *** ORG INIT my invoices
  (defvar invoice-dir "~/org/07-needs/Invoices/")
  (defvar invoice-template-path (expand-file-name "_invoice_template.org" invoice-dir))

  (defun my/invoice-next-number ()
    "Get next sequential invoice number. Invoice numbers are in the format YYYYXXX,
where YYYY is the current year and XXX is a zero-padded sequential counter
modulo 1000. Ex.: 2016001."
    (concat (format-time-string "%Y" (current-time))
            (format "%03d" (% (1+ (string-to-number
                                   (substring (car (last (directory-files
                                                          invoice-dir
                                                          nil
                                                          "^[0-9]+\.org$"))) 4 7))) 1000))))

  (defun my/invoice-get-path (number)
    "Derive invoice file path from invoice NUMBER."
    (format "%s/%s.org" invoice-dir number))

  (defun my/invoice-create (scope-file)
    "Make a new invoice from given file and date range. assume you have to manually complete the invoice."
    (interactive "forg file: ")
    (let ((invoice-number (my/invoice-next-number))
          (current-headline (org-entry-get nil "ITEM"))
          (payment-date (calendar-date-string
                         (calendar-gregorian-from-absolute
                          (+ (calendar-absolute-from-gregorian (calendar-current-date))
                             14))))
          (invoice-date (format-time-string "%m-%d-%Y" (current-time)))
          (invoice-start (org-read-date nil t nil "Choose invoice start" nil "-2Mon"))
          (invoice-end (org-read-date nil t nil "Choose invoice end" nil "-Sun")))
      (find-file (my/invoice-get-path invoice-number))
      (insert-file-contents invoice-template-path)
      (goto-char (point-min))
      (while (search-forward "@INVOICE_NUMBER@" nil t)
        (replace-match invoice-number))
      (goto-char (point-min))
      (while (search-forward "@INVOICE_DATE@" nil t)
        (replace-match invoice-date))
      (goto-char (point-min))
      (while (search-forward "@PAYMENT_DATE@" nil t)
        (replace-match payment-date))
      (goto-char (point-min))
      (while (search-forward "@current_headline@" nil t)
        (replace-match current-headline))
      (goto-char (point-min))
      (while (search-forward "@invoice_start@" nil t)
        (replace-match (format-time-string "%Y-%m-%d" invoice-start)))
      (goto-char (point-min))
      (while (search-forward "@invoice_end@" nil t)
        (replace-match (format-time-string "%Y-%m-%d" invoice-end)))
      (goto-char (point-min))
      (while (search-forward "@scope_file@" nil t)
        (replace-match scope-file))
      (org-update-all-dblocks)))

  (defun my/invoice-create-from-current-buffer-file ()
    "Make a new invoice from current buffer's file and given date range."
    (interactive)
    (my/invoice-create (buffer-file-name)))
;;; *** ORG INIT add screenshot utility command http://stackoverflow.com/a/17438212
  (defun my-org-screenshot ()
    "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
    (interactive)
    (org-display-inline-images)
    (defvar my-myscreenshot_filename
      (concat
       (make-temp-name
        (concat (file-name-nondirectory (buffer-file-name))
                "_imgs/"
                (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (unless (file-exists-p (file-name-directory my-myscreenshot_filename))
      (make-directory (file-name-directory my-myscreenshot_filename)))
                                        ; take screenshot
    (if (eq system-type 'darwin)
        (call-process "screencapture" nil nil nil "-i" my-myscreenshot_filename))
    (if (eq system-type 'gnu/linux)
        (call-process "import" nil nil nil my-myscreenshot_filename))
                                        ; insert into file if correctly taken
    (if (file-exists-p my-myscreenshot_filename)
        (insert (concat "[[file:" my-myscreenshot_filename "]]"))))

;;; *** ORG init tangle and add to agenda on save
  ;; Tangle Org files when we save them
	(defun tangle-on-save-org-mode-file()
		(when (string= (message "%s" major-mode) "org-mode")
			(org-babel-tangle)))

	(add-hook 'after-save-hook 'tangle-on-save-org-mode-file)

	;; Enable the auto-revert mode globally. This is quite useful when you have
	;; multiple buffers opened that Org-mode can update after tangling.
	;; All the buffers will be updated with what changed on the disk.
	(global-auto-revert-mode)

	;; Add Org files to the agenda when we save them
	(defun to-agenda-on-save-org-mode-file()
		(when (string= (message "%s" major-mode) "org-mode")
			(org-agenda-file-to-front)))

	(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;;; ** ORG CONFIG Begin
  :config
  (setq org-yank-adjusted-subtrees t)
	(setq org-fontify-whole-heading-line t)
;;; *** ORG Config: cleanup after export
	(setq-default org-latex-logfiles-extensions (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))
	(setq-default org-latex-remove-logfiles t)




;;; *** ORG config sync bookmarks

  ;; define a datadir mapping for auxilatory host
  (defconst data-dir
    (if (string-match-p (regexp-quote "end") (system-name))
        "/mnt/MODATA"
      "/mnt/DATA"))

;;; function to convert absolute paths from one system to another
  (defun sync-relative-name (file)
    (cond
     ((file-exists-p file)
      file)
     ((string-path-match "^.+/DATA/\\(.+\\)" file)
      (expand-file-name (match-string 1 file) data-dir))
     (t file)))

;;; use function to filter bookmarks
  (advice-add 'bookmark-get-filename :filter-return 'sync-relative-name)

;;; *** ORG Config Babel

  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (shell . t)
     (makefile . t)
     (dot . t)
     (python . t)
     (ledger . t)
     (js . t)
     (restclient . t)
     (emacs-lisp . t)))

;;; **** add a hook function to check if this is trello file, then activate the org-trello minor mode. [#5]
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((filename (buffer-file-name (current-buffer))))
                (when (and filename (string= "trello" (file-name-extension filename)))
                  (org-trello-mode)))))

;;; *** Org CONFIG Babel
  (require 'ob-shell)


;;; *** ORg config: nicer org elipsis
  (setq org-ellipsis "⤵")

;;; *** Org config export backends
  (setq org-export-backends (quote (ascii html icalendar latex md odt)))

;;; *** Org config export
  (setq org-export-in-background t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-latex-classes
        '(("beamer"
           "\\documentclass[presentation]{beamer}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("article"
           "\\documentclass[12pt]{hitec}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  [NO-EXTRA]
  \\settextfraction{0.95}\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("report"
           "\\documentclass[11pt]{report}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("book"
           "\\documentclass[11pt]{book}"
           ("\\part{%s}" . "\\part*{%s}")
           ("\\chapter{%s}" . "\\chapter*{%s}")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
          ("une-article"
           "\\documentclass[a4paper,12pt]{scrartcl}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage[margin=1.5cm]{geometry}
  [EXTRA]\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("une-logo"
           "\\documentclass[a4paper,12pt]{scrartcl}
  [DEFAULT-PACKAGES]
  [PACKAGES]
  \\usepackage[margin=1.5cm]{geometry}
  [EXTRA]
  \\definecolor{unegreen}{HTML}{7AB800}
  \\definecolor{Black}{HTML}{000000}
  \\definecolor{White}{HTML}{FFFFFF}
  \\definecolor{dimgrey}{HTML}{696969}
  \\makeatletter
  \\def\\@maketitle{
   \\noindent \\begin{minipage}[c][4cm][t]{\\linewidth}
     \\colorbox{Black}{%
       \\begin{minipage}[t][4cm][c]{4cm}
       \\flushleft
       \\includegraphics{~/org/07-needs/data/e2/d93aae-bf9f-4f84-9cbb-d58618144046/luis_rosenstrauch_s.png}
     \\end{minipage}}
     \\colorbox{unegreen}{%
       \\begin{minipage}[t][4cm][c]{13.5cm}
         \\flushright
         \\Large \\textbf{\\color{White}{\\@title}} \\\\
          \\vspace{4pt}
         \\small \\color{White}{\\@author} \\\\
         \\small \\color{White}{\\@date}
       \\end{minipage}}
     \\end{minipage}}
  \\makeatother\n"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
          ("old-article" "\\documentclass[11pt]{article}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
        )

;;; *** ORG config: allow linking by id [#1]

  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Update ID file on startup
  ;;(org-id-update-id-locations)


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

  ;; configure wip limit to 2 next tasks
  (setq org-wip-limit 2)
  (setq org-wip-state "NEXT")
;;; *** ORG CONFIG: Directories [#8]
  (setq org-default-notes-file "~/org/home.org")
  (setq org-agenda-files "~/org")
  (setq org-agenda-files (append '("~/org")
                                 ;;(file-expand-wildcards "~/org/boards/*.trello")
                                 (file-expand-wildcards "~/org/01-internal/*/*.org")
                                 (file-expand-wildcards "~/org/02-clients/*/*.org")
                                 (file-expand-wildcards "~/org/03-TribeProjects/*/*.org")
                                 (file-expand-wildcards "~/org/05-start/*/*.org")
                                 (file-expand-wildcards "~/org/06-teams/*/*.org")
                                 (file-expand-wildcards "~/org/07-needs/*/*.org")
                                 (file-expand-wildcards "~/org/08-system/*/*.org")
                                 ;;(file-expand-wildcards "~/org/09-private/*/*.org")

                                 (file-expand-wildcards "~/org/*/*.org")
                                 ;;(file-expand-wildcards "~/org/*/*/*.org")
                                 ))
  (setq org-archive-location "~/org/04-archive/%s_archive::")

;;; *** ORG CONFIG Mobile org


;;; **** ORG MOBILE: the location of your Org files on your local system [#1]
  (setq org-directory "~/org")
;;; **** ORG MOBILE: the name of the file where new notes will be stored [#1]
  (setq org-mobile-inbox-for-pull "~/org/flagged.org")
;;; **** ORG MOBILE: configuration. [#2]
  (setq org-mobile-directory "/rosen_sync@files.rosenstrauch.com:/home/rosen_sync/roSynxcBox/MobileOrg")

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
															 'org-mobile-sync)));
	(defun org-mobile-sync-disable ()
		"disable mobile org idle sync"
		(interactive)
		(cancel-timer org-mobile-sync-timer))
	(org-mobile-sync-enable)

;;; *** ORG CONFIG: CAPTURE [#5]

  (setq org-default-notes-file (concat org-directory "/capture.org"))
  (setq org-capture-templates
        '(("s" "ScreenShot" entry (file+headline "~/org/capture.org" "ScreenShots")
           "* %?\n  %i\n  %a")
          ("t" "Todo" entry (file+headline "~/org/capture.org" "Tasks")
           "* WISH %?\n  %i\n  %a")
          ("f" "Financial entries")
          ("fm" "bankcard" plain
           (file "~/org/07-needs/finance.ledger")
           "%(org-read-date) %^{Payee}
  Liabilities:bankcard
  Expenses:%^{Account}  %^{Amount}
")
          ("fc" "Cash" plain
           (file "~/org/07-needs/finance.ledger")
           "%(org-read-date) * %^{Payee}
  Expenses:Cash
  Expenses:%^{Account}  %^{Amount}
")
;;; **** ORG CONFIG Capture contacts
          ("c" "👤 Contact" entry
           (file+headline "~/contacts.org" "People")
           "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:END:"
           :clock-keep t :kill-buffer t)

          ("O" "Company Contacts" entry
           (file+headline (expand-file-name "contacts.org" org-directory) "Companies")
           "* %(org-contacts-template-name)
 :PROPERTIES:
 :EMAIL: %(org-contacts-template-email)
 :PHONE:
 :ALIAS:
 :NICKNAME:
 :IGNORE:
 :ICON:
 :NOTE:
 :ADDRESS:
 :BIRTHDAY:
 :END:")

;;; **** CAPTURE: Ideas are not tasks                                            [#2]
          ("i" "Idea" entry (file+headline "~/org/ideas.org" "IdeaInbox")
           "* %?\nEntered on %U\n  %i\n  %a")
;;; **** CAPTURE: journal entries do not show up in agenda (but maybe as diary) [#2]
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
;;; **** CAPTURE: calendar entries prompt for date and show up in agenda may or maynot be todos [#2]
          ("D" "Date" entry (file+datetree "~/org/calendar.org")
           "* %?\nEntered on %^T\n  %i\n  %a")
;;; **** CAPTURE: Routines are repeated tasks captured to routines.org [#2]
          ("r" "Routine" entry (file "~/org/routines.org")
           "** TODO %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
;;; **** CAPTURE: LinkLibrary is sorted by capture date in a date tree [#2]
          ("l" "Link" entry (file+olp+datetree "~/org/links.org")
           "* %c %a %x %?\nEntered on %U\n  %i\n" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)
;;; **** CAPTURE: Snippets will need to get refiled if i capture a link it may include a snipptet [#6]
          ("x" "Firefox Capture Template" entry
           (file+headline "~/org/capture.org" "ClippedSnippets")
           "* Snippets %a\n%i\nEntered on %U\n%c\ \nNote: %?\nLink: %l" :prepend t :jump-to-captured t :empty-lines-after 1 :unnarrowed t)))

;;; *** ORG CUSTOM AGENDA Views [#3]

  (setq org-agenda-custom-commands
        '(

          ("S" . "Status")
          ("I" . "Inventory")

          ("D" . "Data")
          ("M" . "Misc")
;;; **** CUSTOM AGENDA: Review Views [#29]
          ("R" . "Review" )
          ("Rw" "Week in review"
           agenda ""
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 0)
            (org-agenda-overriding-header "Week in Review"))
           ("/mnt/DATA/exportedata/org-export/review/week.html"))
          ("Rd" "Day in review"
           agenda ""
           ((org-agenda-span 'day)
            (org-agenda-overriding-header "Week in Review"))
           ("/mnt/DATA/exportedata/org-export/review/day.html"))

          ("Rm" "Month in review"
           agenda ""
           ((org-agenda-span 'month)
            (org-agenda-start-day "01")
            (org-read-date-prefer-future nil)
            (org-agenda-overriding-header "Month in Review"))
           ("/mnt/DATA/exportedata/org-export/review/month.html"))

;;; **** CUSTOM AGENDA: Priority views [#24]
          ("c" . "Priority views")
          ("ca" "#A"  ((tags "PRIORITY=\"A\""
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

          ("S" . "Status")
          ("I" . "Inventory")
          ("Q" . "Quests")
          ("D" . "Data")
          ("M" . "Misc")
;;; **** CUSTOM AGENDA: Review Views [#29]
          ("R" . "Review" )
          ("Rw" "Week in review"
           agenda ""
           ((org-agenda-span 'week)
            (org-agenda-start-on-weekday 0)
            (org-agenda-overriding-header "Week in Review"))
           ("/mnt/DATA/exportedata/org-export/review/week.html")
           )


          ("Rd" "Day in review"
           agenda ""
           ((org-agenda-span 'day)
            (org-agenda-overriding-header "Week in Review"))

           ("/mnt/DATA/exportedata/org-export/review/day.html")
           )

          ("Rm" "Month in review"
           agenda ""
           ((org-agenda-span 'month)
            (org-agenda-start-day "01")
            (org-read-date-prefer-future nil)
            (org-agenda-overriding-header "Month in Review"))

           ("/mnt/DATA/exportedata/org-export/review/month.html")
           )

;;; **** CUSTOM AGENDA: Priority views [#24]
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

;;; **** CUSTOM AGENDA: Quests [#1]
          ("Q" . "Quests") ;;; gives label to "Q"
          ("Qo" "Todos tagged as Issues" tags-todo "-SCHEDULED={.+}/!+ISSUE")
;;; ***** quests dont contain issues and boards
;;; ***** quests that are estimated, are not scheduled, are not DONE.
;;; ***** quests to estimate next [#1]
          ("Qi" "[i]nserted unestimated tasks" tags-todo "Effort<1-SCHEDULED={.+}/!-DONE" )
;;; ***** quests which have estimates [#1]
          ("Qs" "[s]chedule next tasks" tags-todo "Effort>1-SCHEDULED={.+}/!-DONE")
;;; ***** quests scheduled for today [#10]
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
;;; **** CUSTOM AGENDA: queries/searches [#1]
          ("F" . "Find - Custom queries/searches") ;;; gives label to "Q"
;;; ***** sparse tree for next string [#5]
          ("Fn" "Next in file" occur-tree "NEXT")
          ("Fi" "Issue search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/issues/*.issues"))))
          ("FA" "Archive search" search ""
           ((org-agenda-files (file-expand-wildcards "~/org/04-archive/*.org_archive"))))
;;; ***** match tagged headlines that are not todos [#3]
          ("K" "Knowledge" tags "+{.*}+TODO=\"\"|+{.*}+TODO=\"DONE\""
           ((org-tags-match-list-sublevels 'indented)
            (org-agenda-sorting-strategy '(tag-up))))
;;; ***** match orphan headlines (the ones without tag or todo) [#6]
          ("O" "Orphans" tags "-{.*}+TODO=\"\""
           ((org-tags-match-list-sublevels 'indented)))
          ("P" . "Projects") ;;; gives label to "P"
          ("P1" "Internal Block"
           ((tags "PRJ" ((org-agenda-overriding-header "\nInternal Projects\n------------------\n")))
            (tags-todo "Effort>1-SCHEDULED={.+}/!-DONE" ((org-agenda-overriding-header "\Estimated Unscheduled Internal Tasks\n------------------\n"))))
;;; ***** only look in internal org files [#1]
           ((org-agenda-files (file-expand-wildcards "~/org/01-internal/*/*"))
;;; ***** show columns [#2]
            (org-agenda-overriding-columns-format "%TODO %7EFFORT %PRIORITY %100ITEM 100%TAGS")
            (org-agenda-view-columns-initially t)))
;;; **** CUSTOM AGENDA: Customer Agenda [#20]
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
;;; **** CUSTOM AGENDA: Printed Agenda [#22]

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
;;; Custom Agenda end [#1]
          ))

;;; *** ORG CONFIG: Agenda [#1]

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

  (setq org-columns-default-format
        "%45ITEM %3TODO %5Effort(Time){:} %3PRIORITY %TAGS %6CLOCKSUM(Clock)")
                                        ; Set default column view headings: Task Effort Clock_Summary
                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit"))))

;;; *** ORG CONFIG: Clocking [#3]

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

;;; ** ORG projects (publishing) [#19]
;;;; Switches off use of time-stamps when publishing. I would prefer to publish everything every time
  (setq org-publish-use-timestamps-flag nil)
  (setq org-publish-project-alist
        '(
          ("org" :components ("orgsystem-html" "org-styles" "orgsystem-pdf"))

          ("org-styles"
           :base-directory "~/org/styles"
           :recursive t
           :base-extension "css\\|js"
           :publishing-directory "/mnt/DATA/exportedata/org_published/"
           :publishing-function org-publish-attachment)
          ("orgsystem-pdf"
           :base-directory "~/org/08-system"
           :base-extension "org"
           :publishing-directory "/mnt/DATA/exportedata/org_published/full/pdf/system"
           :section-numbers nil
           :with-toc nil
           :exclude "//^_.org$"
           :recursive t
           :publishing-function org-latex-publish-to-pdf)
          ("orgsystem-html"
           :base-directory "~/org/08-system"
           :publishing-directory "/mnt/DATA/exportedata/org_published/full/html/system"
           :base-extension "org"
           :recursive t
           :exclude "^_[a-z]"
           :section-numbers 3
           :with-toc t
           :toc t
           :n t
           :H 6
           :auto-sitemap t
           :h 6
           :toc 6
           :sitemap-filename "sitemap.org"
           :sitemap-title "Sitemap"
           :publishing-function org-html-publish-to-html)))

;;; ** ORG Diary/Journal [#2]
  (setq org-agenda-diary-file "~/org/journal.org")

;;; ** ORG Tags [#11]
																				;(setq org-tag-alist '((:startgroup . nil)
																				;                      ("@work" . ?w) ("@home" . ?h)
																				;                      ("@errands" . ?t)
																				;                      ("@BUY" . ?t)
																				;                      ("@meeting" . ?m)
																				;                      ("@phone" . ?c)
																				;                      (:endgroup . nil)
																				;                      ("PRJ" . ?e)
																				;                      ("TEAM" . ?g)
																				;                      ("@laptop" . ?l) ("@pc" . ?p)))

;;; ** ORG Tasks [#4]
  (setq org-log-redeadline (quote time))
  (setq org-log-done (quote time))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-reschedule (quote time))
;;; *** ORG TASKS don't show tasks that are scheduled or have deadlines in the normal todo list [#2]
  (setq org-agenda-todo-ignore-deadlines (quote all))
  (setq org-agenda-todo-ignore-scheduled (quote all))

;;; *** ORG TODO Keywords [#15]
  (setq org-todo-keywords
        (quote (
                (sequence "WISH(w)"  "TODO(t)" "HOLD(h@/!)" "NEXT(n)" "|" "CANCELED(c)" "DONE(d)")
                ;;; do we treat ideas as special type of task
                (sequence "RESEARCH(r)" "DEFINE(f)" "|" "KNOWLEDGE(k)")
                )))
  (setq org-todo-keyword-faces
        (quote (("TODO" :foreground "red" :weight bold)
                ("NEXT" :foreground "pink1" :weight bold)
                ("WISH" :foreground "medium sea green" :weight bold)
                ("INSERT" :foreground "medium sea green" :weight bold)
                ("DONE" :background "darkgrey" :weight bold :box (:line-width 2 :style released-button))
                ("HOLD" :foreground "orange" :weight bold)
                ("CANCELLED" :foreground "forest green" :weight bold))))
;;; *** ORG Project tags [#49]
  (setq org-tags-exclude-from-inheritance '("PRJ" "TEAM")
        org-stuck-projects '("+PRJ/-HOLD-INSERT-DONE"
                             ("NEXT" "TODO") ("@BUY")))
  ;;; Refile
  (setq org-reverse-note-order t)
  (setq org-refile-use-outline-path nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-cache nil)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 7))))

  (defun org-refile-to-datetree (&optional file)
    "Refile a subtree to a datetree corresponding to it's timestamp.

    The current time is used if the entry has no timestamp. If FILE
    is nil, refile in the current file."
    (interactive "f")
    (let* ((datetree-date (or (org-entry-get nil "TIMESTAMP" t)
                              (org-read-date t nil "now")))
           (date (org-date-to-gregorian datetree-date))
           )
      (save-excursion
        (with-current-buffer (current-buffer)
          (org-cut-subtree)
          (if file (find-file file))
          (org-datetree-find-date-create date)

          (org-narrow-to-subtree)
          (show-subtree)
          (org-end-of-subtree t)
          (newline)
          (goto-char (point-max))
          (org-paste-subtree 4)
          (widen)
          ))
      )
    )


  (setq org-blank-before-new-entry nil)
  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "a)"))))

;;; * Endof Org config
  )
(provide 'init)
;;; init.el ends here
