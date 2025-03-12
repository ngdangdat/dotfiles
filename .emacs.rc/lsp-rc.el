;; Set up autoload for the lsp command
(autoload 'lsp "lsp-mode" nil t)

;; Add hooks to start LSP automatically in these modes
(add-hook 'python-mode-hook #'lsp)
(add-hook 'js-mode-hook #'lsp)
(add-hook 'typescript-mode-hook #'lsp)

;; Configuration settings
(setq lsp-enable-symbol-highlighting t)
(setq lsp-enable-xref t)
