(use-package olivetti
  :ensure t
  :hook prog-mode
  :custom (olivetti-body-width nil))

;; (define-globalized-minor-mode global-olivetti-mode olivetti-mode
;;   (lambda () (olivetti-mode 1)))

;; (global-olivetti-mode 1)

(provide 'setup-olivetti)
