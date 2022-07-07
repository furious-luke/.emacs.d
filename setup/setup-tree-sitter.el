(use-package tree-sitter
  :ensure t
  :defer nil
  :config (global-tree-sitter-mode)
  :hook ((tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t
  :defer nil
  :after tree-sitter)
  ;; :config
  ;; (tree-sitter-require 'tsx)
  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(provide 'setup-tree-sitter)
