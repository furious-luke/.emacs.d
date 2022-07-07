;;
;; Python environment integrations.
;;

(use-package pyenv-mode
  :ensure t)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;; (use-package poetry
;;   :ensure t
;;   :config (add-hook 'prog-mode-hook #'poetry-tracking-mode))

(provide 'setup-python)
