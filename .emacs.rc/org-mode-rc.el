(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'org-roam-dailies-capture-today)

(add-to-list 'org-modules 'org-habit)
;; org roam
(setq org-roam-directory "~/Dropbox/org/roam")
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
    '(("d" "default" entry
       "* TODO %? :daily:\n SCHEDULE: %t\n"
       :empty-lines 1
       :target (file+head "%<%Y-%m-%d>.org"
                          "#+title: %<%Y-%m-%d>\n"))))

;; set org folder, probably in Dropbox
(setq org-agenda-files (directory-files-recursively "~/Dropbox/org/" "\\.org$"))
(setq org-default-notes-file  "~/Dropbox/org/rambling.org")
(setq org-export-backends '(md))

;; statuses
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")))

;; capture
(setq org-capture-templates
      '(("r" "RSL" entry (file "~/Dropbox/org/projects/rsl.org")
         "* TODO %? :work:rsl:\n SCHEDULED: %t\n")
      ("a" "ADU" entry (file "~/Dropbox/org/projects/adu.org")
         "* TODO %? :work:adu:\n SCHEDULED: %t\n")
      ("d" "AVZ" entry (file "~/Dropbox/org/projects/avz.org")
       "* TODO %? :work:avz:\n SCHEDULED: %t\n")
      ("h" "HABIT" entry (file "~/Dropbox/org/projects/habit.org")
         "* TODO %? :habit:\n SCHEDULED: %t\n")
      ("p" "PSN" entry (file "~/Dropbox/org/projects/psn.org")
         "* TODO %? :psn:\n SCHEDULED: %t\n" :empty-lines 1)))
