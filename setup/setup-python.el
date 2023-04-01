;;
;; Python environment integrations.
;;

(use-package python
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil))

(use-package pyenv-mode
  :ensure t)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

(use-package poetry
  :ensure t
  :defer t
  :custom
  (blacken-allow-py36 t)
  (blacken-skip-string-normalization t)
  :hook (python-mode-hook . blacken-mode))
;; :config (add-hook 'prog-mode-hook #'poetry-tracking-mode))

(provide 'setup-python)
