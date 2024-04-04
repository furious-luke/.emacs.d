(use-package multiple-cursors
  :ensure t
  :bind ("C->" . mc/mark-next-like-this)
  :bind ("C-<" . mc/mark-previous-like-this))

(provide 'setup-multiple-cursors)
