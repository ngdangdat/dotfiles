;;; .emacs -*- lexical-binding: t; -*-
(setq custom-file "~/.emacs.custom.el")

;; load extra files
(load "~/.emacs.rc/rc.el")
;; (add-to-list 'load-path "")

;; define custom functions
(defun ndd/custom-whitespace-mode-hook ()
  "Disable whitespace mode for some file extensions"
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
 ;; organize files
 'no-littering
 'magit)

;; appearance
(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-14"))
(setq inhibit-startup-screen 1)
(setq display-line-numbers-type 'relative)
(setq whitespace-line-column 80)
(setq indent-tabs-mode -1)
(transient-mark-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(load-theme 'leuven-dark)
(global-whitespace-mode 1)
(global-display-line-numbers-mode 1)


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
(keymap-global-set "C-s" #'swiper-isearch)
(keymap-global-set "C-r" #'swiper-isearch-backward)
(keymap-global-set "C-c C-r" #'ivy-resume)

;; load custom file
(when (file-exists-p custom-file)
  (load-file custom-file))
