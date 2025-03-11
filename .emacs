(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(load "~/.emacs.rc/rc.el")

;;; To tell emacs to not litter
(rc/require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backup/"))))

(setq inhibit-startup-screen 1)

;;; Appearance
(load-theme 'wombat t)
(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-14"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode)
(global-whitespace-mode)

(load-file custom-file)
