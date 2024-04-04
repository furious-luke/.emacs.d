(use-package flymake
  :ensure nil
  :elpaca nil
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error))

;; (use-package flymake-ruff
;;   :ensure t
;;   :hook (eglot-managed-mode . flymake-ruff-load))

(provide 'setup-flymake)
