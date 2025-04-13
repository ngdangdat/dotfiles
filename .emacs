;;; .emacs -*- lexical-binding: t; -*-
(setq custom-file "~/.emacs.custom.el")

;; custom require, install packages if it's not installed and refresh once only
(load "~/.emacs.rc/rc.el")

;; define custom functions
(defun ndd/custom-whitespace-mode-hook ()
  "Customize whitespace mode for some file extensions"
  (let ((file-name (buffer-file-name)))
    (when file-name
      (when (or (string-match "\\.org$" file-name)
                (string-match "\\.md$" file-name))
        (whitespace-mode -1))
      (when (string-match "\\.py" file-name)
        (setq-local whitespace-line-column 120))
      (when (string-match "\\.json" file-name)
        (setq-local whitespace-line-column 1000))
      (when (or (string-match "\\.go$" file-name)
                (string-match "Caddyfile*" file-name))
        (setq-local indent-tabs-mode t))
      )))
(add-hook 'find-file-hook 'ndd/custom-whitespace-mode-hook)

;; packages
(rc/require
 ;; auto completion
 'ivy
 'swiper
 'counsel
 'projectile
 'company
 'company-posframe
 ;; organize files
 'no-littering
 ;; appearance
 'doom-themes
 ;; org-mode
 'org
 'org-clock-convenience
 ;; programming
 'python-mode
 'go-mode
 'lsp-pyright
 'typescript-mode
 'js2-mode
 'lsp-mode
 'lsp-ui
 )

;; appearence
(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-14"))
(setq inhibit-startup-screen 1)
(setq display-line-numbers-type 'relative)
(setq whitespace-line-column 80)
(setq-default indent-tabs-mode nil)
(transient-mark-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'leuven-dark)
(global-whitespace-mode 1)
(global-display-line-numbers-mode 1)
(setq doom-theme 'doom-bluloco-dark)
(load-theme doom-theme t)

;; no littering, emacs
(eval-and-compile
 (setq auto-save-file-name-transforms
       `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
 (setq backup-directory-alist
       `(("." . ,(no-littering-expand-var-file-name "backup/"))))
 (require 'no-littering))

;; autocomplete
(require 'swiper)
(require 'counsel)
(ivy-mode)
(setopt ivy-use-virtual-buffers t)
(setopt ivy-use-selectable-prompt t)
;; swiper
(keymap-set global-map "C-s" #'swiper-isearch)
(keymap-set global-map "C-r" #'swiper-isearch-backward)
(keymap-set global-map "C-c C-r" #'ivy-resume)
;; projectile
(projectile-mode)
(setq projectile-completion-system 'ivy)
(keymap-set projectile-mode-map "C-c p" projectile-command-map)

;; org
(require 'org)
(require 'org-agenda)
(require 'org-clock-convenience)
(load-file "~/.emacs.rc/org.el")

;; lsp


;; load custom file
(when (file-exists-p custom-file)
  (load-file custom-file))
