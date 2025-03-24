;; Set up autoload for the lsp command
(autoload 'lsp "lsp-mode" nil t)

;; Add hooks to start LSP automatically in these modes
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

;; Python
(add-hook 'python-mode-hook #'(lambda()
                                (require 'lsp-pyright)
                                (lsp)))

;; Javascript
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

;; Configuration settings
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-xref t)
