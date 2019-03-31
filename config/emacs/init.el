;;; package --- RosenEmacsInit
;;; Commentary:
;;; --- i actually keep my Emacs config in el format.
;;; --- by now i have extracted certain long pieces into their own files which i load from here.


;;; Code:
;;; * GLOBAL Emacs Settings

;;; ** GLOBAL Load custom Lisp from others [#1] placed there by fresh
(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;;; ** GLOBAL Store Customizations elsewhere [#6]
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file) (with-temp-buffer (write-file custom-file)))
(load custom-file)

;;; ** GLOBAL KEYMAPs
(load "~/.emacs.d/greenmacs-keymaps.el")

;;; ** GLOBAL Configure Settings
(load-file "~/.emacs.d/global-settings.el")

(defun load-directory (dir)
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))
(load-directory "~/.emacs.d/elisp/")



;;; ** STYLE

(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-20"))

;;; *** STYLE background color


(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (width . 106)
            (height . 60)
            (background-color . "DarkGreen")
            (left . 50)
            (top . 50)))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))

(setq default-frame-alist initial-frame-alist)


;;; *** STYLE: Autorecognize Page Breaks [#6]
(setq page-delimiter
      (rx bol (or "\f" ";;;")
          (not (any "#")) (* not-newline) "\n"
          (* (* blank) (opt ";" (* not-newline)) "\n")))
;; Expanded regexp:
;; "^;;;[^#].*\n\\(?:[[:blank:]]*\\(?:;.*\\)?\n\\)*"
;;; *** STYLE: highlight Matching parenthesis
(show-paren-mode 1)
(set-frame-font "9x15")
(setq line-spacing '0.25)
(column-number-mode 1)


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
  (yank))
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
;;; * Use Package Elm Mode [#2]
(use-package elm-mode
  :ensure t)

;;; * Use Package Json Mode [#2]
(use-package json-mode
  :ensure t)

;;; * Use Package Make Mode [#2]
(use-package make-mode
  :ensure t)

;;; * Use Package dockerfile-mode [#2]
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;;; * Use Package: Markdown mode [#8]
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;;; * Use Package: Web mode [#20]
(use-package web-mode
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
        web-mode-enable-current-element-highlight t
        web-mode-markup-indent-offset 2)
  :ensure t)

;;; * Use Package message-mode
(use-package message-mode
  :commands message-mode

  :no-require t
  :config
  (message-default-mail-headers "
Cc:
X-Message-SMTP-Method: sendmail
")
  ;; This is needed to allow msmtp to do its magic:
  (setq message-sendmail-f-is-evil 't)

  ;;need to tell msmtp which account we're using
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq mail-specify-envelope-from t)
  (setq mail-envelope-from 'header)
  (setq message-sendmail-envelope-from 'header)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)

  ;;use msmtp instead of sendmail
  (setq sendmail-program "/usr/bin/msmtp")

  (setq message-kill-buffer-on-exit t)
  (setq message-alternative-emails
        (regexp-opt '("green@endeavour.localdomain" "luis.rosenstrauch@gmail.com")))

  (add-hook 'message-mode-hook 'turn-on-flyspell 'append))

;;; * Use Package Yaml Mode [#2]
(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (defun yaml-mode-syntax-propertize-function (beg end)
    (save-excursion
      (goto-char beg)
      (while (search-forward "#" end t)
        (save-excursion
          (forward-char -1)
          (if (bolp)
              (put-text-property (point) (1+ (point))
                                 'syntax-table (string-to-syntax "<"))
            (forward-char -1)
            (when (looking-at "[ \t]")
              (forward-char 1)
              (put-text-property (point) (1+ (point))
                                 'syntax-table (string-to-syntax "<")))))))) )

;;; * Use Package: ledger-mode
(use-package ledger-mode
  :disabled t
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

;;; * Load Ide for git
(load-file "~/.emacs.d/setup-git-ide.el")

;;; * Load Ide for php
(load-file "~/.emacs.d/setup-php-ide.el")
;;; * Load Ide for js
(load-file "~/.emacs.d/setup-js-ide.el")
;;; * Load Ide for python
(load-file "~/.emacs.d/setup-python-ide.el")
;;; * Load Ide for git
(load-file "~/.emacs.d/setup-git-ide.el")
;;; * Load Ide for vuejs
(load-file "~/.emacs.d/setup-vue-ide.el")
;;; * Load Ide for java
(load-file "~/.emacs.d/setup-java-ide.el")

;;; * Use Package org-invoice
;;;;   - https://github.com/jbranso/.emacs.d/blob/master/lisp/init-org.org#org-invoice
(use-package org-invoice)

;;; * Use Package: csv-export
(use-package org-clock-csv
  :ensure t)

;;; * Use Package: org-screenshot
(use-package org-attach-screenshot
  :ensure t)

;;; * Use Package yasnippet-mocha
(use-package mocha-snippets
  :ensure t)

;;; * Use Package: Restclient [#3]
(use-package restclient
  :ensure t
  :defer t)

;;; * Use Package: Xclip
;;;;   - for integrating emacs kill-ring with gnome clipboard [#4]
(use-package xclip
  :ensure t
  :init (xclip-mode 1))

;;; * Use Package: habits [#4]
(use-package habitica
  :bind (("C-c C-h" . habitica-tasks))
  :ensure t)

;;; * Use Package: org-attach-screenshot [#3]
(use-package org-attach-screenshot
  :bind
  (("C-c C-s" . org-attach-screenshot)))

;;; * Use Package yasnippet
(use-package yasnippet
  :ensure t
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))


;;; * Use Package: emacs-w3m
(use-package w3m
  :config
  (setq browse-url-browser-function 'w3m-browse-url)
  (autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t))

;;; * Use Package Makefile ob [#2]
  (use-package ob-makefile
    :config
    (setq org-src-preserve-indentation t))
;;; * Use Package alert  [#2]
  (use-package alert
    :commands (alert)
    :init
    (setq alert-default-style 'notifier))

;;; * Use Package: emacs-fasd
  (use-package fasd
    :bind(("\C-c f" . fasd-find-file))
    :config (setq global-fasd-mode 1)
    (setq fasd-enable-initial-prompt nil)
    :ensure t)

;;; * Use Package: mocha
  (use-package mocha
    :bind(
          ("C-c m P" . mocha-test-project)
          ("C-c m f" . mocha-test-file)
          ("C-c m p" . mocha-test-at-point))
    :ensure t)

;;; * Use Package: Notmuch
  (use-package notmuch
    :commands (notmuch)
    :bind (("C-. m" . notmuch)
           :map notmuch-search-mode-map
           ;; bind 'r' to reply-all, and 'R' to reply
           ("r" . notmuch-search-reply-to-thread)
           ("R" . notmuch-search-reply-to-thread-sender)
           ("§" . notmuch-search-filter-by-date)))

;;; * Use Package: babel restclient
  (use-package ob-restclient
    :ensure t
    :defer t)

;;; * Use Package: flycheck position
  (use-package flycheck-pos-tip
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))  )

;;; * Use Package: Org Trello [#3]
  (use-package org-trello
    :mode "\\.trello$"
    :interpreter "org-mode"
    :ensure t)

;;; * Use Package: Htmlize for exporting agenda [#8]
  (use-package htmlize
    :commands (htmlize-buffer
               htmlize-file
               htmlize-many-files
               htmlize-many-files-dired
               htmlize-region)
    :ensure t)


;;; * Use Package tramp
  (use-package tramp
    :ensure t
    :config
    (setq tramp-default-method "ssh")
;;;		(setq tramp-shell-prompt-pattern "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")
    (setq tramp-shell-prompt-pattern "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
    (setq tramp-chunksize 500)
    (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash") ) )

;;; * Use Package org-contacts
;;;;  - https://emacs.stackexchange.com/a/17553
  (use-package org-contacts
    :config
    (setq org-contacts-files `(,(expand-file-name "contacts.org" org-directory)))
    (setq org-contacts-icon-use-gravatar nil)
    (setq org-contacts-address-property "CITY")
    :ensure org-plus-contrib)

;;; * Use Package Sh-script
;;;;  - for editing shell files [#9]
  (use-package sh-script
    :mode (("\\.*bashrc$" . sh-mode)
           ("\\.*bash_profile" . sh-mode)
           ("\\.zsh" . sh-mode)
           ("\\.bash" . sh-mode)         )
    :config
    (setq-default sh-indentation 2
                  sh-basic-offset 2))

;;; * Use Package slack
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t)
    :config

    ;; (slack-register-team
    ;;  :name "emacs-slack"
    ;;  :default t
    ;;  :client-id "aaaaaaaaaaa.00000000000"
    ;;  :client-secret "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    ;;  :token "xoxs-sssssssssss-88888888888-hhhhhhhhhhh-jjjjjjjjjj"
    ;;  :subscribed-channels '(test-rename rrrrr)
    ;;  :full-and-display-names t)

    ;; (slack-register-team
    ;;  :name "test"
    ;;  :client-id "3333333333.77777777777"
    ;;  :client-secret "cccccccccccccccccccccccccccccccc"
    ;;  :token "xoxs-yyyyyyyyyy-zzzzzzzzzzz-hhhhhhhhhhh-llllllllll"
    ;;  :subscribed-channels '(hoge fuga))
    )


;;; * Use Package: helm
  (use-package helm
    :bind (("M-x" . helm-M-x)
           ("M-<f5>" . helm-find-files)
           ([f10] . helm-buffers-list)
           ([S-f10] . helm-recentf))
    :ensure t)

;;; * Use Package company
  (use-package company
    :ensure t
    :config
    (global-company-mode)
    (setq company-tooltip-limit 10)
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0)
    (setq company-echo-delay 0)
    (setq company-minimum-prefix-length 2)
    (setq company-require-match nil)
    (setq company-selection-wrap-around t)
    (setq company-tooltip-align-annotations t)
    ;; (setq company-tooltip-flip-when-above t)
    (setq company-transformers '(company-sort-by-occurrence)) ; weight by frequency
    (define-key company-active-map (kbd "M-n") nil)
    (define-key company-active-map (kbd "M-p") nil)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
    (define-key company-active-map (kbd "<backtab>") 'company-select-previous))

  (use-package helm-company
    :ensure t
    :config
    ;; http://emacs.stackexchange.com/questions/10431/get-company-to-show-suggestions-for-yasnippet-names
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))
;;; * Use Package: Outshine
;;;   > to fold my `init.el' like an org file [#31]
  (use-package outshine
    :ensure t
    :diminish outline-minor-mode
    :init
    (defvar outline-minor-mode-prefix "C-ß")
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
    :config
    (setq outshine-use-speed-commands t))
;;; * Use Package: Outline

  (use-package outline
    :ensure t
    :defines outline-mode-map
    :commands (outline-mode outline-minor-mode)
    :config
    (bind-keys :map outline-mode-map
               :prefix-map my-outline-prefix
               :prefix "\C-ß"
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


;;; * Use Package irc
  (use-package erc
    :commands (erc-tls erc-next-channel-buffer my-erc-md-all-but-emacs bitlbee)
    :init
    (defun bitlbee ()
      (interactive)
      (if (get-buffer "&bitlbee")
          (switch-to-buffer (get-buffer "&bitlbee"))
        (let ((default-directory "~"))
          (erc :server "localhost" :port 6667 :nick "rosenstrauch"))))

    (defun bitlbee-netrc-identify ()
      "Auto-identify for Bitlbee channels using authinfo or netrc.

    The entries that we look for in netrc or authinfo files have
    their 'port' set to 'bitlbee', their 'login' or 'user' set to
    the current nickname and 'server' set to the current IRC
    server's name.  A sample value that works for authenticating
    as user 'keramida' on server 'localhost' is:

    machine localhost port bitlbee login keramida password supersecret"
      (interactive)
      (when (string= (buffer-name) "&bitlbee")
        (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
                                                             :host erc-server
                                                             :user (erc-current-nick)
                                                             :port "bitlbee"))
                                  :secret))
               (password (if (functionp secret)
                             (funcall secret)
                           secret)))
          (erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))

    ;; Enable the netrc authentication function for &biblbee channels.
    (add-hook 'erc-join-hook 'bitlbee-netrc-identify)

    (defun my-erc-md-all-but-emacs ()
      "Minimal distraction for all channels except #emacs"
      (interactive)
      (setq erc-track-priority-faces-only
            (remove "##rosenchat" (my-erc-joined-channels))))

    (defun my-erc-joined-channels ()
      "Return all the channels you're in as a list.  This does not include queries."
      (save-excursion
        ;; need to get out of ERC mode so we can have *all* channels returned
        (set-buffer "*scratch*")
        (mapcar #'(lambda (chanbuf)
                    (with-current-buffer chanbuf (erc-default-target)))
                (erc-channel-list erc-process))))


    (define-minor-mode ncm-mode "" nil
      (:eval
       (let ((ops 0)
             (voices 0)
             (members 0))
         (maphash (lambda (key value)
                    (when (erc-channel-user-op-p key)
                      (setq ops (1+ ops)))
                    (when (erc-channel-user-voice-p key)
                      (setq voices (1+ voices)))
                    (setq members (1+ members)))
                  erc-channel-users)
         (format " %S/%S/%S" ops voices members))))


    (defvar erc-channels-to-visit nil
      "Channels that have not yet been visited by erc-next-channel-buffer")
    (defun erc-next-channel-buffer ()
      "Switch to the next unvisited channel. See erc-channels-to-visit"
      (interactive)
      (when (null erc-channels-to-visit)
        (setq erc-channels-to-visit
              (remove (current-buffer) (erc-channel-list nil))))
      (let ((target (pop erc-channels-to-visit)))
        (if target
            (switch-to-buffer target))))

    :config
    (setq
     erc-hide-list '("JOIN" "PART" "QUIT")
     erc-insert-timestamp-function 'erc-insert-timestamp-left
     erc-timestamp-format "[%H:%M] "
     erc-timestamp-only-if-changed-flag nil
     erc-truncate-mode t)

    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#wiki" "#nethack" "##rosenchat")
            ("oftc.net" "#bitlbee")))
    (erc-tls :server "irc.freenode.net" :port 6697 :nick "rosenstrauch")
    (erc-tls :server "irc.oftc.net" :port 6697 :nick "rosenstrauch")

    ;; Kill buffers for channels after /part
    (setq erc-kill-buffer-on-part t)

    ;; Kill buffers for private queries after quitting the server
    (setq erc-kill-queries-on-quit t)
    ;; Kill buffers for server messages after quitting the server
    (setq erc-kill-server-buffer-on-quit t)

    ;; open query buffers in the current window
    (setq erc-query-display 'buffer)
    (setq erc-log-insert-log-on-open nil)
    (setq erc-log-channels t)
    (setq erc-log-channels-directory "~/.irclogs/")
    (if (not (file-exists-p erc-log-channels-directory))
        (mkdir erc-log-channels-directory t))

    (setq erc-save-buffer-on-part t)
    (setq erc-hide-timestamps nil)
    (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)

    (setq erc-keywords '("resolve" "rosenstrauch"))
    (erc-match-mode 1)
    :bind ( :map erc-mode-map
                 ("C-c C-a" . erc-next-channel-buffer) ))

;;; * Use Package flycheck
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
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)  )

;;;   Use Package: Org id
  (use-package org-id
    :config
    (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
    (org-id-update-id-locations))

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

           ("C-c j" . org-clock-goto) ;;; jump to current task from anywhere
           ("C-c C-w" . org-refile)
           ("C-c d" . org-refile-to-datetree)
           ("C-c is" . my-org-screenshot)
           ;;("C-c oc" . org-contacts)
           ("C-<" . org-insert-structure-template))
;;; ** ORG INIT [#1]
    :init
;;; *** Org mode INIT github issue new url

    (load-file "~/.emacs.d/org-init-functions.el")

;;; ** Org mode CONFIG Begin
    :config
    (setq org-yank-adjusted-subtrees t)
    (setq org-fontify-whole-heading-line t)
    (setq org-blank-before-new-entry nil)
    (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                                ("*" . "-")
                                                ("1." . "-")
                                                ("1)" . "a)"))))

;;; *** Org mode Config: nicer org elipsis
    (setq org-ellipsis "⤵")
;;; *** Org mode Config: Directories [#8]
    (setq org-default-notes-file "~/org/capture.org")
    (setq org-agenda-files (file-expand-wildcards "~/org/*.org"))

    (setq org-archive-location "~/org/archive.org::* From %s") ;; http://doc.endlessparentheses.com/Var/org-archive-location.html
    (setq org-directory "~/org")
    (setq org-default-notes-file (concat org-directory "/capture.org"))
;;; ** Org mode Config Diary/Journal [#2]
    (setq org-agenda-diary-file "~/org/journal.org")

    ;; configure wip limit to 2 next tasks
    (setq org-wip-limit 2)
    (setq org-wip-state "NEXT")

  ;;; Refile
    (setq org-reverse-note-order t)
    (setq org-refile-use-outline-path nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-refile-use-cache nil)
    (setq org-refile-targets '((org-agenda-files . (:maxlevel . 7))))

    (setq org-columns-default-format
          "%45ITEM %3TODO %5Effort(Time){:} %3PRIORITY %TAGS %6CLOCKSUM(Clock)")
                                        ; Set default column view headings: Task Effort Clock_Summary
                                        ; global Effort estimate values
                                        ; global STYLE property values for completion
    (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                        ("STYLE_ALL" . "habit"))))

;;; *** Org mode Config Babel


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

;;; **** Org mode config trello
;;; --- add a hook function to check if this is trello file, then activate the org-trello minor mode. [#5]
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))

;;; *** Org mode Config export
    (load-file "~/.emacs.d/org-export-settings.el")

;;; *** Org mode Config mobile
    (load-file "~/.emacs.d/org-mobile-sync.el")

;;; *** Org mode Config: CAPTURE [#5]
    (load-file "~/.emacs.d/org-capture-settings.el")

;;; *** Org mode Config Agenda
    (load-file "~/.emacs.d/org-agenda-settings.el")

;;; *** Org mode Config: Clocking [#3]
    (load-file "~/.emacs.d/org-clock-settings.el")

;;; ** Org mode Config projects (publishing) [#19]
    (load-file "~/.emacs.d/org-publish-settings.el")

;;; ** Org mode Config Tags [#11]
    (setq org-tag-alist '((:startgroup . nil)))

;;; ** Org mode Config Tasks [#4]
    (setq org-log-redeadline (quote time))
    (setq org-log-done (quote time))
    (setq org-enforce-todo-dependencies t)
    (setq org-log-reschedule (quote time))
;;; *** Org mode Config TASKS
;;;        --- don't show tasks that are scheduled or have deadlines in the normal todo list [#2]
    (setq org-agenda-todo-ignore-deadlines (quote all))
    (setq org-agenda-todo-ignore-scheduled (quote all))

;;; *** Org mode Config TODO Keywords [#15]
    (setq org-todo-keywords
          (quote ((sequence "WISH(w)"  "TODO(t)" "HOLD(h@/!)" "NEXT(n)" "|" "CANCELED(c)" "DONE(d)")
                ;;; do we treat ideas as special type of task
                  (sequence "RESEARCH(r)" "DEFINE(f)" "|" "KNOWLEDGE(k)")                )))

    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("NEXT" :foreground "pink1" :weight bold)
                  ("WISH" :foreground "medium sea green" :weight bold)
                  ("INSERT" :foreground "medium sea green" :weight bold)
                  ("DONE" :background "darkgrey" :weight bold :box (:line-width 2 :style released-button))
                  ("HOLD" :foreground "orange" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold))))

;;; *** Org mode Config Project tags [#49]
    (setq org-tags-exclude-from-inheritance '("PRJ" "TEAM")
          org-stuck-projects '("+PRJ/-HOLD-INSERT-DONE"
                               ("NEXT" "TODO") ("@BUY")))


;;; *** Org mode Config sync bookmarks

;;;       --- define a datadir mapping for auxilatory host
    (defconst data-dir
      (if (string-match-p (regexp-quote "end") (system-name))
          "/mnt/MODATA"
        "/mnt/DATA"))

;;;; --- function to convert absolute paths from one system to another
    (defun sync-relative-name (file)
      (cond
       ((file-exists-p file)
        file)
       ((string-path-match "^.+/DATA/\\(.+\\)" file)
        (expand-file-name (match-string 1 file) data-dir))
       (t file)))

;;; use function to filter bookmarks
    (advice-add 'bookmark-get-filename :filter-return 'sync-relative-name)

;;; * Endof Org config
    )
  (provide 'init)
;;; init.el ends here
