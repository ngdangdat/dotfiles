(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(load "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/org-mode-rc.el")

;;; To tell emacs to not litter
(rc/require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backup/"))))

(rc/require 'org)
(rc/require 'org-roam)
(rc/require 'lsp-mode)
;; programming languages
(rc/require
 'go-mode
 'python-mode
 'typescript-mode
 'js2-mode)
(load-file "~/.emacs.rc/lsp-rc.el")

(setq inhibit-startup-screen 1)
(setq display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil)

;;; ido
(rc/require 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

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
(transient-mark-mode)


(load-file custom-file)
