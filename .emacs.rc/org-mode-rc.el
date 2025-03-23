(require 'org)
(require 'org-agenda)
(require 'org-clock-convenience)
(add-to-list 'org-modules 'org-habit)

(add-hook 'org-mode-hook (lambda ()
                           (setq-local whitespace-style nil) ; don't use whitespace in org mode
                           (whitespace-mode -1)))
; Set key map
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c <tab>") 'ndd/org-agenda)
(global-set-key (kbd "C-c SPC") 'ndd/org-agenda-done)

; Set key map for the org-agenda-mode-map
(define-key org-agenda-mode-map (kbd "p") #'ndd/org-process-inbox)
(define-key org-agenda-mode-map (kbd "i") #'org-agenda-clock-in)
(define-key org-agenda-mode-map (kbd "I") #'ndd/clock-in-advance)
(define-key org-mode-map (kbd "C-c i") #'org-insert-structure-template)

(defun ndd/org-agenda-clock-keymap ()
  "Create keymap for org-clock-convenience in agenda mode."
  (define-key org-agenda-mode-map (kbd "C-M-<up>") #'org-clock-convenience-timestamp-up)
  (define-key org-agenda-mode-map (kbd "C-M-<down>") #'org-clock-convenience-timestamp-down)
  (define-key org-agenda-mode-map (kbd "C-M-f") #'org-clock-convenience-fill-gap)
  (define-key org-agenda-mode-map (kbd "C-M-b") #'org-clock-convenience-fill-gap-both))
(add-hook 'org-agenda-mode-hook #'ndd/org-agenda-clock-keymap)

; Configurations
(setq org-habit-show-habits-only-for-today t)
(setq org-agenda-block-separator "----"
      org-agenda-start-with-log-mode t)
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-directory "~/.orgs/")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq ndd/org-agenda-project-dir (concat org-directory "gtd/projects"))
(setq ndd/org-agenda-project-agenda-files (directory-files-recursively ndd/org-agenda-project-dir "\\.org$"))
(setq org-export-backends '(md))
(setq org-columns-default-format
      "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
(setq org-tag-alist '(("@work" . ?w)
                      ("@personal" . ?p)))
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-targets
      (mapcar (lambda (file) (cons file '(:level . 1)))
              ndd/org-agenda-project-agenda-files))
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((alltodo ""
                   ((org-agenda-overriding-header "Inbox")
                    (org-agenda-files `(,(expand-file-name "gtd/inbox.org" org-directory)))))
          (agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-start-day nil)
                   (org-deadline-warning-days 365)))
          (todo "NEXT"
                ((org-agenda-overriding-header "In Progress")
                 (org-agenda-files (cons
                                    (expand-file-name "gtd/repeats.org" org-directory)
                                    ndd/org-agenda-project-agenda-files))))
          (todo "TODO|HOLD"
                ((org-agenda-overriding-header "Queued")
                 (org-agenda-files ndd/org-agenda-project-agenda-files)))
          (todo "TODO"
                ((org-agenda-overriding-header "Repeats")
                 (org-agenda-files `(,(expand-file-name "gtd/repeats.org" org-directory)))))))
        ;; check to see if it's being used
        ("d" "Done" todo "DONE"
         ((org-agenda-overriding-header "What Should I Be Proud Of Today?")
          (org-agenda-span 'day)
          (org-agenda-start-day nil)
          (org-agenda-files ndd/org-agenda-project-agenda-files)))))

(defvar ndd/org-current-effort "1:00"
  "Current effort of org item.")

(defun ndd/org-set-effort (effort)
  "Set effort for the current item."
  (interactive
   (list (read-string (format "Effort: [%s]: " ndd/org-current-effort) nil nil ndd/org-current-effort)))
  (setq ndd/org-current-effort effort)
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
           (funcall-interactively 'org-set-effort nil ndd/org-current-effort)
           (end-of-line 1)
           (setq newhead (org-get-heading)))
         (org-agenda-change-all-lines newhead hdmarker))))

(defun ndd/org-process-inbox ()
  "Called in org-agenda mode to process inbox in bulk."
  (interactive)
  (org-agenda-bulk-mark-regexp ":INBOX:")
  (ndd/bulk-process-entries))

(defun ndd/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let* ((entries (reverse org-agenda-bulk-marked-entries))
             (processed 0)
             (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if pos
                (progn
                  (goto-char pos)
                  (let (org-loop-over-headlines-in-active-region)
                    (funcall 'ndd/org-agenda-process-inbox-item))
                  (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
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
(defun ndd/org-agenda-done ()
  (interactive)
  (org-agenda nil "d"))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))

(setq org-capture-templates
      `(("i" "Inbox" entry (file "~/.orgs/gtd/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
         ("r" "Repeat" entry (file ,(expand-file-name
                                     "gtd/repeats.org"
                                     org-directory))
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))))
