(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

;; Configurations
(setq inhibit-startup-screen 1)

(load-file custom-file)
