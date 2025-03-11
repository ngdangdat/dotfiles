;; again and again
;; test again
;; test hehehehe
(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(load "~/.emacs.rc/rc.el")

(rc/require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backup/"))))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; Configurations
(setq inhibit-startup-screen 1)

(load-file custom-file)
