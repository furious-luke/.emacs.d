;; (use-package tex
;;   :elpaca auctex)

(defvar package-list '(auctex))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-and-compile
  (defun auctex-load-path ()
    (car (file-expand-wildcards "~/.emacs.d/elpa/auctex-*"))))

(use-package tex
  :ensure nil
  :load-path (lambda () (auctex-load-path))
  :init (load "auctex.el" nil t t))

;; (defun ded:elpaca-build-dir (p)
;;   "Return the elpaca build directory for package symbol p"
;;   (-first-item
;;    (f-directories elpaca-builds-directory
;;   	              (lambda (dir) (string-match-p (concat "^" (symbol-name p) "$") (f-filename dir))))))

;; (use-package auctex
;;   :elpaca (auctex :pre-build (("./autogen.sh")
;;                               ("./configure" "--without-texmf-dir" "--with-lispdir=.")
;;                               ("make")
;;                               ("install-info" "doc/auctex.info" "doc/dir")
;;                               ("install-info" "doc/preview-latex.info" "doc/dir")))
;;   :mode (("\\.tex\\'" . TeX-latex-mode)
;;          ("\\.tex\\.erb\\'" . TeX-latex-mode)
;;          ("\\.etx\\'" . TeX-latex-mode))
;;   :init
;;   (add-to-list 'Info-additional-directory-list (f-join (ded:elpaca-build-dir 'auctex) "doc"))
;;   (add-hook 'tex-mode-hook
;;             (lambda ()
;;               (load "auctex.el")
;;               (setq TeX-command-extra-options "-shell-escape")))
;;   :config
;;   (setq-default TeX-global-PDF-mode 1)
;;   (setq-default  preview-scale-function 1.5)
;;   (setq TeX-auto-save t
;;         TeX-parse-self t
;;         default-truncate-lines t
;;         TeX-save-query nil
;;         TeX-source-correlate-method 'synctex))

(provide 'setup-latex)
