;; ;; Returns the parent directory containing a .project.el file, if any,
;; ;; to override the standard project.el detection logic when needed.
;; (defun zkj-project-override (dir)
;;   (let ((override (locate-dominating-file dir ".project")))
;;     (if override
;;       (cons 'vc override)
;;       nil)))

;; (use-package project
;;   :ensure nil
;;   :elpaca nil
;;   :config
;;   (add-hook 'project-find-functions #'zkj-project-override))

;; (use-package project-x
;;   :ensure t
;;   :elpaca (:host github :repo "karthink/project-x")
;;   :after (project)
;;   :config
;;   (setq project-x-save-interval 600)
;;   (project-x-mode 1)
;;   (setq project-find-functions nil)
;;   (add-hook 'project-find-functions 'project-x-try-local 90))

(provide 'setup-project)
