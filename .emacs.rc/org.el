;;; .org.el -*- lexical-binding: t; -*-
(add-to-list 'org-modules 'org-habit)
(add-hook 'org-mode-hook (lambda()
                           (setq-local whitespace-style nil)
                           (whitespace-mode 0)
                           (org-bullets-mode 1)))

;; Configurations
;; setup org text file path
(setq org-directory "~/.orgs/gtd/")
(setq ndd/org-agenda-project-dir (concat org-directory "projects"))
(setq ndd/org-agenda-project-agenda-files
      (directory-files-recursively ndd/org-agenda-project-dir "\\.org$"))
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
;; setup org-log
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)
;; org refiles
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      (mapcar (lambda (file) (cons file '(:level . 1)))
              ndd/org-agenda-project-agenda-files))
;; chores
(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                 (todo . " %i %-12:c %-6e")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-start-with-log-mode t)
(setq org-tag-alist '(("@work" . ?w)
                      ("@personal" . ?p)))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-hide-emphasis-markers t)

(setq org-todo-keywords'((sequence
                          "TODO(t)"
                          "NEXT(n)"
                          "HOLD(h)"
                          "|"
                          "DONE(d)"
                          "CANCELLED(c)")))
;;customize org-agenda appearance
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((alltodo ""
                   ((org-agenda-overriding-header "Inbox")
                    (org-agenda-files `(,(expand-file-name
                                          "inbox.org"
                                          org-directory)))))
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-day nil)
                   (org-deadline-warning-days 365)))
          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-files (cons
                                    (expand-file-name
                                     "repeats.org"
                                     org-directory)
                                    ndd/org-agenda-project-agenda-files))))
          (todo "TODO|HOLD"
                ((org-agenda-overriding-header "Queued")
                 (org-agenda-files ndd/org-agenda-project-agenda-files)))
          (todo "TODO"
                ((org-agenda-overriding-header "Repeats")
                 (org-agenda-files `(,(
                                       expand-file-name
                                       "repeats.org"
                                       org-directory)))))))))

;; functions
(defun ndd/org-agenda-clock-keymap ()
  "Create keymap for org-clock-convenience in agenda mode."
  (define-key
   org-agenda-mode-map
   (kbd "C-M-<up>") #'org-clock-convenience-timestamp-up)
  (define-key
   org-agenda-mode-map
   (kbd "C-M-<down>") #'org-clock-convenience-timestamp-down)
  (define-key
   org-agenda-mode-map
   (kbd "C-M-g f") #'org-clock-convenience-fill-gap)
  (define-key
   org-agenda-mode-map
   (kbd "C-M-g b") #'org-clock-convenience-fill-gap-both))

(defvar ndd/latest-set-effort "1:00"
  "Latest effort used. Default is 1:00.")

(defun ndd/org-set-effort (effort)
  "Set effort for the current item."
  (interactive
   (list (read-string (format
                       "Effort: [%s]: "
                       ndd/latest-set-effort)
                      nil nil ndd/latest-set-effort)))
  (setq ndd/latest-set-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
       (org-with-remote-undo buffer
         (with-current-buffer buffer
           (widen)
           (goto-char pos)
           (org-show-context 'agenda)
           (funcall-interactively 'org-set-effort nil ndd/latest-set-effort)
           (end-of-line 1)
           (setq newhead (org-get-heading)))
         (org-agenda-change-all-lines newhead hdmarker))))
(defun ndd/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(defun ndd/org-process-inbox ()
  "Process inbox in bulk"
  (interactive)
  (org-agenda-bulk-mark-regexp ":INBOX:")
  (ndd/bulk-process-entries)
  (org-save-all-org-buffers)
  )

(defun ndd/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let* ((entries (reverse org-agenda-bulk-marked-entries))
             (processed 0)
             (skipped 0))
        (dolist (e entries)
          (let ((pos
                 (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if pos
                (progn
                  (goto-char pos)
                  (let (org-loop-over-headlines-in-active-region)
                    (funcall 'ndd/org-agenda-process-inbox-item))
                  (when (or (memq
                             'org-add-log-note
                             (default-value 'post-command-hook))
                            (memq 'org-add-log-note post-command-hook))
                    (org-add-log-note))
                  (cl-incf processed))
              (cl-incf skipped))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries (skipped %d)" processed skipped))))

(defun ndd/org-agenda-process-inbox-item ()
  "Process a single item in inbox."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'ndd/org-set-effort)
   (org-agenda-refile nil nil t)))

(defun ndd/advance-todo ()
  (org-todo 'right)
  (org-agenda-redo)
  (remove-hook 'org-clock-in-hook #'ndd/advance-todo))

(defun ndd/clock-in-advance ()
  (interactive)
  (add-hook 'org-clock-in-hook #'ndd/advance-todo)
  (org-agenda-clock-in))

(defun ndd/org-agenda ()
  (interactive)
  (org-agenda nil " "))


;; map keys here
;; map: org
(keymap-set global-map "C-c l" 'org-store-link)
(keymap-set global-map "C-c c" 'org-capture)
(keymap-set global-map "C-c <tab>" 'ndd/org-agenda)
(keymap-set org-agenda-mode-map "p" #'ndd/org-process-inbox)
(keymap-set org-agenda-mode-map "i" #'org-agenda-clock-in)
(keymap-set org-agenda-mode-map "I" #'ndd/clock-in-advance)
(add-hook 'org-agenda-mode-hook #'ndd/org-agenda-clock-keymap)

(setq org-capture-templates
      `(("i" "Inbox" entry (file "~/.orgs/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
         ("r" "Repeat" entry (file ,(expand-file-name
                                     "repeats.org"
                                     org-directory))
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))))


;; org-roam
(setq org-roam-directory "~/.orgs/roam/")
;; org-roam: key map
(org-roam-db-autosync-mode)
(keymap-set global-map "C-c r l" #'org-roam-buffer-toggle)
(keymap-set global-map "C-c r f" #'org-roam-node-find)
(keymap-set global-map "C-c r g" #'org-roam-graph)
(keymap-set global-map "C-c r i" #'org-roam-node-insert)
(keymap-set global-map "C-c r c" #'org-roam-capture)
(keymap-set global-map "C-c r j" #'org-roam-dailies-capture-today)
