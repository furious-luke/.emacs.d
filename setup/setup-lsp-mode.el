;;
;; Language Server Protocol.
;;

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package yasnippet
  :ensure t
  :defer t
  :hook
  (prog-mode . yas-minor-mode)
  (text-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :ensure t)

(use-package lsp-treemacs
  :after lsp-mode
  :ensure t)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide 'setup-lsp-mode)
