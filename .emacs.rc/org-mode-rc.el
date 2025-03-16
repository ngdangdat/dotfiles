(require 'org)

(add-hook 'org-mode-hook (lambda ()
                           (setq-local whitespace-style nil)
                           (whitespace-mode -1)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'org-roam-dailies-capture-today)
(global-set-key (kbd "C-c i") 'org-insert-structure-template)

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
         "* TODO %? :work:rsl:\nSCHEDULED: %t\n")
      ("a" "ADU" entry (file "~/Dropbox/org/projects/adu.org")
         "* TODO %? :work:adu:\nSCHEDULED: %t\n")
      ("d" "AVZ" entry (file "~/Dropbox/org/projects/avz.org")
       "* TODO %? :work:avz:\nSCHEDULED: %t\n")
      ("h" "HABIT" entry (file "~/Dropbox/org/projects/habit.org")
         "* TODO %? :habit:\nSCHEDULED: %t\n")
      ("p" "PSN" entry (file "~/Dropbox/org/projects/psn.org")
         "* TODO %? :psn:\nSCHEDULED: %t\n" :empty-lines 1)))
