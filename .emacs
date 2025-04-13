;;; .emacs -*- lexical-binding: t; -*-
(setq custom-file "~/.emacs.custom.el")
(package-initialize)
;; custom require, install packages if it's not installed and refresh once only
(load "~/.emacs.rc/rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(setq vc-follow-symlinks t)

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
 'doom-modeline
 ;; org-mode
 'org
 'org-clock-convenience
 ;; programming
 'exec-path-from-shell
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
(setq-default tab-width 4)
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
(setq doom-modeline-buffer-file-name-style 'truncate-nil)
(setq doom-modeline-total-line-number t)
(setq doom-modeline-vcs-icon t)
(setq doom-modeline-vcs-max-length 15)
(setq doom-modeline-workspace-name t)
(doom-modeline-mode 1)

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
;; preload environment variables
(require 'exec-path-from-shell)
(dolist (var '("PATH" "PYENV_ROOT" "WORKON_HOME"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)
(require 'pyvenv)
(load-file "~/.emacs.rc/lsp.el")

;; load custom file
(when (file-exists-p custom-file)
  (load-file custom-file))
