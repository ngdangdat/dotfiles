(setq custom-file "~/.emacs.custom.el")

;; load extra files
(load "~/.emacs.rc/rc.el")

;; appearance
(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-14"))
(setq inhibit-startup-screen 1)
(transient-mark-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'leuven-dark)

;; no littering please
(rc/require 'no-littering)
(eval-and-compile
 (setq auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
 (setq backup-directory-alist
       `(("." . ,(no-littering-expand-var-file-name "backup/"))))
 (require 'no-littering))


;; autocomplete
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)

(when (file-exists-p custom-file)
  (load-file custom-file))
