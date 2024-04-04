(use-package company
  :ensure t
  :init (setq company-idle-delay 0.3
              company-minimum-prefix-length 1)
  :config (global-company-mode)
  :bind
  ("TAB" . company-indent-or-complete-common))
  ;; ("<tab>" . company-complete)
  ;; ("TAB" . indent-for-tab-command))

(provide 'setup-company)
