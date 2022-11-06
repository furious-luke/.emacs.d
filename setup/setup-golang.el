(use-package go-mode
  :ensure t
  :config (add-hook 'go-mode-hook
                    (lambda ()
                      (setq-default indent-tabs-mode 1)
                      (setq-default tab-width 2))))

(provide 'setup-golang)
