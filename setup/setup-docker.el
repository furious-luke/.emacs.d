;; dockerfile-mode.
(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

;; docker-compose-mode.
(use-package docker-compose-mode
  :ensure t)

;; docker.
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(provide 'setup-docker)
