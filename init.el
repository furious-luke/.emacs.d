(add-to-list 'load-path "~/.emacs.d/custom/")

;;
;; Autosave/backups.
;;

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq gc-cons-threshold 100000000)

;;
;; Remove menu/tool bars.
;;

(if window-system
    (menu-bar-mode -1))
(if window-system
    (tool-bar-mode -1))

;;
;; Whitespace instead of tabs.
;;

(setq-default indent-tabs-mode nil)

;;
;; Install MELPA.
;;

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;
;; Install use-package.
;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;
;; Use PATH from shell.
;;

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Respect .envrc files.
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;
;; Set default font with ligatures.
;;

(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono"
                    :height 120)  ;; 1/10 pt

;; TODO: This doesn't work *at all*.
;; (use-package fira-code-mode
;;   :ensure t
;;   :hook prog-mode) ;; Enables fira-code-mode automatically for programming major modes

;;
;; Enable recent files mode.
;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;
;; Bookmarks.
;;

(unless (package-installed-p 'eshell-bookmark)
  (package-install 'eshell-bookmark))
(use-package eshell-bookmark
  :config
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

;;
;; Company completion.
;;

(use-package company
  :ensure t)

;;
;; Dracula theme.
;;

(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))
(load-theme 'dracula t)

;;
;; Icons all 'round!
;;

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;;
;; Projectile.
;;

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-to-list 'projectile-globally-ignored-directories "node_modules")

;;
;; Helm.
;;

(unless (package-installed-p 'helm)
  (package-install 'helm))
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(setq helm-find-files-ignore-thing-at-point t)

;;
;; Helm/Projectile integration.
;;

(unless (package-installed-p 'helm-projectile)
  (package-install 'helm-projectile))
(require 'helm-projectile)
(helm-projectile-on)

;;
;; Helm AG.
;;

(unless (package-installed-p 'ag)
  (package-install 'ag))
(unless (package-installed-p 'helm-ag)
  (package-install 'helm-ag))

;;
;; Avy.
;;

(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :bind ("C-'" . avy-goto-word-1)
  :bind ("C-:" . avy-goto-char-timer))

;;
;; Which-key.
;;

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;
;; Python environment integrations.
;;

(use-package pyenv-mode
  :ensure t)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))

;;
;; Graphql mode
;;

(use-package graphql-mode
  :ensure t)

;; TODO: Is currently broken.
;; (use-package poetry
;;   :ensure t)
;;  :config (add-hook 'prog-mode-hook #'poetry-tracking-mode))

;;
;; Language Server Protocol.
;;

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :config (setq lsp-pyls-plugins-flake8-enabled t)
          (setq lsp-pyls-plugins-pyflakes-enabled nil)
          (setq lsp-pyls-plugins-pycodestyle-enabled nil)
          (setq lsp-pyls-configuration-sources ["flake8"])
          (setq lsp-prefer-capf t)
          (setq read-process-output-max (* 1024 1024))
          (setq lsp-enable-indentation nil)
          (setq lsp-file-watch-threshold 10000)
          (push "[/\\\\]node_modules\\'" lsp-file-watch-ignored)
          (setq lsp-eldoc-hook nil)
          (setq lsp-signature-auto-activate nil)
          (setq lsp-signature-doc-lines 1)
          (lsp-register-custom-settings
           '(("pyls.plugins.pyls_mypy.enabled" t t)
             ("pyls.plugins.pyls_mypy.live_mode" nil t)
             ("pyls.plugins.pyls_black.enabled" t t)
             ("pyls.plugins.pyls_isort.enabled" t t)))
  :hook ((python-mode . lsp)
         (js-mode . lsp)
;; 	 (web-mode . lsp)
;; 	 (rjsx-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :config (setq lsp-ui-sideline-show-diagnostics nil)
          (setq lsp-ui-sideline-show-hover nil)
          (setq lsp-ui-sideline-show-code-actions t)
          (setq lsp-ui-doc-enable t)
          (setq lsp-ui-doc-include-signature t)
          (setq lsp-ui-doc-frame-mode t)
          (setq lsp-ui-doc-max-width 80)
          (setq lsp-ui-doc-max-height 25)
  :bind (("C-c w" . lsp-ui-doc-focus-frame)
         ("C-c q" . lsp-ui-doc-focus-frame)))
;;   :config (setq lsp-ui-sideline-show-code-actions nil)
;;   :commands lsp-ui-mode)
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)
;; (use-package helm-lsp
;;   :ensure t
;;   :commands helm-lsp-workspace-symbol)
;; ;; ;; if you are ivy user
;; ;; ;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs
;;   :ensure t
;;   :commands lsp-treemacs-errors-list)

;; ;; Eslint integration.
;; (setq lsp-eslint-server-command 
;;       '("node" 
;;         "/home/luke/.vscode/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js" 
;;         "--stdio"))

;; optionally if you want to use debugger
;; TODO
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;
;; Docker.
;;

;; dockerfile-mode.
(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; docker-compose-mode.
(unless (package-installed-p 'docker-compose-mode)
  (package-install 'docker-compose-mode))
(use-package docker-compose-mode)

;; docker.
(unless (package-installed-p 'docker)
  (package-install 'docker))
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;
;; Git.
;;

;; Magit.
(unless (package-installed-p 'magit)
  (package-install 'magit))
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Git gutter (diff-hl)
(unless (package-installed-p 'diff-hl)
  (package-install 'diff-hl))
(require 'diff-hl)
(global-diff-hl-mode 1)
(diff-hl-flydiff-mode 1)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;;
;; Groovy
;;

(use-package groovy-mode
  :ensure t)

;;
;; JavaScript.
;;

(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . js-mode))

;; rjsx-mode
;; (unless (package-installed-p 'rjsx-mode)
;;   (package-install 'rjsx-mode))
;; (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
(setq js-indent-level 2)
;; (setq js2-strict-missing-semi-warning nil)
;; (setq js2-missing-semi-one-line-override nil)

;; web-mode
;; (unless (package-installed-p 'web-mode)
;;   (package-install 'web-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;; (setq web-mode-content-types-alist
;;   '(("jsx" . "\\.js[x]?\\'")))
;; (setq web-mode-markup-indent-offset 2)
;; (setq web-mode-css-indent-offset 2)
;; (setq web-mode-code-indent-offset 2)

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (before-save . tide-format-before-save)))

;; (setq tide-format-options '(:indentSize 2 :tabSize 2))
(setq typescript-indent-level 2)

;;
;; JSON.
;;

(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))
;; (add-to-list 'auto-mode-alist '("\\.eslintrc\\'" . json-mode))

;;
;; Terraform.
;;

(use-package terraform-mode
  :ensure t)

;;
;; LUA.
;;

(use-package lua-mode
  :ensure t)

;;
;; Line numbers.
;;

(add-hook 'prog-mode-hook 'linum-mode)

;;
;; Linting with Flycheck.
;;

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode)
;;   :config (setq flycheck-python-flake8-executable "flake8"))
;; (unless (package-installed-p 'flycheck)
;;   (package-install 'flycheck))
;; (global-flycheck-mode)
;; (setq flycheck-python-flake8-executable "flake8")

;; disable jshint since we prefer eslint checking
;; (setq-default flycheck-disabled-checkers
;; 	      (append flycheck-disabled-checkers
;; 		      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'web-mode)

;;
;; Multiple cursors.
;;

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(use-package multiple-cursors
  :ensure t
  :bind ("C->" . mc/mark-next-like-this)
  :bind ("C-<" . mc/mark-previous-like-this))

;;
;; Markdown mode.
;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-flydiff-mode t)
 '(helm-completion-style 'emacs)
 '(helm-mode t)
 '(magit-pull-arguments nil)
 '(markdown-command "pandoc" t)
 '(org-tags-column -119)
 '(package-selected-packages
   '(groovy-mode csv-mode flycheck poetry ibuffer-projectile company-lsp lsp-treemacs helm-lsp lsp-ui lsp-mode terraform-mode lua-mode py-isort markdown-mode multiple-cursors eshell-bookmark eshell-booknmark docker-compose-mode docker exec-path-from-shell json-mode rjsx-mode diff-hl magit dockerfile-mode ag helm-ag helm-projectile use-package projectile helm dracula-theme)))

;;
;; 256 colors in Eshell.
;;

;; (unless (package-installed-p 'xterm-color)
;;   (package-install 'xterm-color))
;; (add-hook 'eshell-before-prompt-hook
;;           (lambda ()
;;             (setq xterm-color-preserve-properties t)))
;; (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
;; (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

;;
;; Sidebar (project-explorer).
;;

;; (use-package project-explorer
;;   :ensure t)

;; (use-package dired
;;   :ensure nil
;;   :config
;;   (progn
;;     (setq insert-directory-program "/bin/ls")))

;;
;; Sidebar (treemacs)
;;

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (treemacs--find-python3))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;
;; PlantUML
;;

(use-package plantuml-mode
  :ensure t
  :config (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar")
          (setq plantuml-default-exec-mode 'jar))
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;;
;; Open file tabs
;;

;; (use-package centaur-tabs
;;   :ensure t
;;   :config
;;   :bind
;;   ("C-<prior>" . centaur-tabs-backward)
;;   ("C-<next>" . centaur-tabs-forward))
;; (setq centaur-tabs-style "bar")
;; (setq centaur-tabs-set-bar 'left)
;; (setq centaur-tabs-set-icons t)
;; ;; (setq centaur-tabs-gray-out-icons 'buffer)
;; (setq centaur-tabs-set-modified-marker 0)
;; (setq centaur-tabs-height 48)
;; (centaur-tabs-mode t)
;; (centaur-tabs-headline-match)
;; (centaur-tabs-group-by-projectile-project)

;;
;; Python isort.
;;

;; (use-package py-isort
;;   :ensure t)
  ;; :config
  ;; (add-hook 'before-save-hook 'py-isort-before-save))


;;
;; Org mode
;;

(setq org-src-tab-acts-natively t)

;;
;; Code compass
;;

(use-package async)
(use-package dash)
(use-package f)
(use-package s)
(use-package simple-httpd)
(use-package code-compass
  :load-path "~/.emacs.d/lisp")

;;
;; Automatic stuff.
;;


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
