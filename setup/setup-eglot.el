(use-package eglot
  :ensure nil
  :elpaca nil)
  ;; TODO: If this is enabled, finding files within a project breaks.
  ;;:hook (prog-mode . eglot-ensure))

(use-package eldoc-box
  :ensure t
  :after (eglot)
  ;; :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :init (setq eldoc-echo-area-prefer-doc-buffer t
              eldoc-echo-area-use-multiline-p nil)
  :bind
  ("C-h ." . eldoc-box-help-at-point))

(provide 'setup-eglot)
