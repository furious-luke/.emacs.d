(use-package go-mode
  :ensure t
  ;;:hook (go-ts-mode . (lambda () (setq-default tab-width 4)))
  :init (setq go-ts-mode-indent-offset 4))
;;   :config (add-hook 'go-mode-hook)
;;   (lambda ()
;;     (setq-default indent-tabs-mode 1)
;;     (setq-default tab-width 2)))

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(provide 'setup-golang)
