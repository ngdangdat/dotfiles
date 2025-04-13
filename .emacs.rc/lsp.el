;;; .lsp.el -*- lexical-binding: t; -*-
;; autoload lsp
(autoload 'lsp "lsp-mode" nil t)

;; golang
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; python
(add-hook 'python-mode-hook #'(lambda()
                                (require 'lsp-pyright)
                                (lsp)))
