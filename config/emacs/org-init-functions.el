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
