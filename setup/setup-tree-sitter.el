(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install 'prompt))

;; (use-package tree-sitter
;;   :ensure t
;;   :demand t
;;   :config (global-tree-sitter-mode)
;;   :hook ((tree-sitter-after-on . tree-sitter-hl-mode)))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :demand t
;;   :after tree-sitter)
;;   ;; :config
;;   ;; (tree-sitter-require 'tsx)
;;   ;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

;; (elpaca-wait)

(provide 'setup-tree-sitter)
