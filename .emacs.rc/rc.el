;;; .lsp.el -*- lexical-binding: t; --*
;; Customized require package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once ()
  (when (not rc/package-contents-refreshed)
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-package (package)
  (when (not (package-installed-p package))
	(rc/package-refresh-contents-once)
	(package-install package)))

(defun rc/require (&rest packages)
  (dolist (package packages)
    (rc/require-package package)))
