;;; init.el --- My Emacs configuration

;;; Commentary:
;;; Main configuration file for Emacs

;;; Code:

;; ===================================
;; BASIC SETUP
;; ===================================
(setq custom-file "~/.emacs.custom.el")
(package-initialize)

(add-to-list 'load-path "~/.emacs.local/")

;; Core functionality
(load "~/.emacs.rc/rc.el")  ;; Custom require function

;; Tell Emacs not to litter the filesystem
(rc/require 'no-littering)
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `(("." . ,(no-littering-expand-var-file-name "backup/"))))

;; Basic interface settings
(setq inhibit-startup-screen 1)
(setq display-line-numbers-type 'relative)
(setq-default indent-tabs-mode nil)

;; ===================================
;; APPEARANCE
;; ===================================
(add-to-list 'custom-theme-load-path "~/.emacs.themes")
(rc/require 'doom-themes)
(require 'doom-themes)
(setq doom-theme 'doom-tokyo-night)
(load-theme doom-theme t)

(rc/require 'doom-modeline)
(doom-modeline-mode 1)

(add-to-list 'default-frame-alist `(font . "JetBrainsMono Nerd Font-14"))
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode)
(global-whitespace-mode)
(setq-default tab-width 4)
(setq whitespace-line-column 120)
(transient-mark-mode)
(global-visual-line-mode)
(global-set-key (kbd "M-s M-s") 'replace-string)

;; ===================================
;; PATH & ENVIRONMENT
;; ===================================
;; Set up pyenv paths
(setq exec-path (append exec-path (list (expand-file-name "~/.pyenv/bin")
                                        (expand-file-name "~/.pyenv/shims")
                                        (expand-file-name "/usr/local/go/bin")
                                        (expand-file-name "~/go/bin"))))

(setenv "PATH" (concat (expand-file-name "~/.pyenv/bin") ":"
                       (expand-file-name "~/.pyenv/shims") ":"
                       (expand-file-name "/usr/local/go/bin") ":"
                       (expand-file-name "~/go/bin") ":"
                       (getenv "PATH")))
(setenv "WORKON_HOME" "~/.pyenvs")
;; ===================================
;; COMPLETION FRAMEWORK
;; ===================================
;; IDO mode setup
(rc/require 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

;; ===================================
;; DEVELOPMENT TOOLS
;; ===================================
;; Project management
(rc/require 'projectile)
(projectile-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ido)

;; WIP fuzzy finder
(require 'frgrep)

;; Python environment
(rc/require 'pyenv-mode)
(require 'pyvenv)

;; LSP (Language Server Protocol)
(rc/require 'lsp-mode)
(rc/require 'lsp-ui)
(rc/require 'company)
(require 'company-clang)
;; (push 'company-lsp company-backends)
(load-file "~/.emacs.rc/lsp-rc.el")

;; Programming languages
(rc/require
 'go-mode
 'python-mode
 'lsp-pyright
 'typescript-mode
 'js2-mode)

;; ===================================
;; ORGANIZATION TOOLS
;; ===================================
;; Org mode
(rc/require
 'org
 'org-clock-convenience
 'org-gcal
 'plstore)
(load "~/.emacs.rc/org-mode-rc.el")

;; ===================================
;; LOAD CUSTOM FILE
;; ===================================
(when (file-exists-p custom-file)
  (load-file custom-file))
;;; init.el ends here
