(add-to-list 'load-path (concat user-emacs-directory "setup/"))

(require 'setup-elpaca)
(require 'setup-general)
(require 'setup-aesthetics)
(require 'setup-project)
(require 'setup-flymake)
(require 'setup-eglot)
(require 'setup-olivetti)
(require 'setup-tree-sitter)
(require 'setup-consult)
(require 'setup-vertico)
(require 'setup-orderless)
(require 'setup-marginalia)
(require 'setup-company)
;; (require 'setup-python)
(require 'setup-typescript)
(require 'setup-golang)
(require 'setup-protobuf)
;; (require 'setup-css)
;; (require 'setup-sql)
;; (require 'setup-dart)
(require 'setup-markdown)
(require 'setup-multiple-cursors)
(require 'setup-yaml)
(require 'setup-magit)
(require 'setup-docker)
;; (require 'setup-latex)

;;
;; Features
;;

;; Highlight regions of buffers for visibility.
;; (use-package pulsar
;;   :ensure t
;;   :config
;;   (setq pulsar-pulse-on-window-change 1)
;;   :hook
;;   (after-init . pulsar-global-mode))

;; Help working with parens.
;; TODO: It's more annoying than usefule, I think.
;; (use-package smartparens
;;   :ensure t
;;   ;; :bind
;;   ;; ("M-<backspace>" . sp-unwrap-sexp)
;;   ;; ("M-<left>" . sp-forward-barf-sexp)
;;   ;; ("M-<right>" . sp-forward-slurp-sexp)
;;   ;; ("M-S-<left>" . sp-backward-slurp-sexp)
;;   ;; ("M-S-<right>" . sp-backward-barf-sexp)
;;   :hook
;;   (after-init . smartparens-global-mode)
;;   (wdired-mode . smartparens-mode)
;;   :custom
;;   (sp-highlight-pair-overlay nil)
;;   (sp-highlight-wrap-overlay nil)
;;   (sp-highlight-wrap-tag-overlay nil)
;;   :config
;;   ;; (show-paren-mode 0)
;;   (require 'smartparens-config))

;;
;; Linting with Flycheck.
;;

;; (use-package flycheck
;;   :ensure t
;;   :config
;;   (setq flycheck-global-modes '(not org-mode))
;;   :init
;;   (global-flycheck-mode))

;;
;; Bookmarks and annotations.
;;

;; Enable annotations for bookmarks. NOTE: This is not needed unless
;; we want the annotation to be forced when creating bookmarks.
;; (setq bookmark-use-annotations t)

;; Integrate with eshell. TODO: Do I need this anymore?
;; (use-package eshell-bookmark
;;   :ensure t
;;   :config
;;   (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

;;
;; Completion system.
;;

;; (use-package selectrum
;;   :ensure t
;;   :init
;;   (selectrum-mode +1))

;; (use-package vertico
;;   :init
;;   (vertico-mode)
;;   ;; Different scroll margin
;;   ;; (setq vertico-scroll-margin 0)
;;   ;; Show more candidates
;;   ;; (setq vertico-count 20)
;;   ;; Grow and shrink the Vertico minibuffer
;;   ;; (setq vertico-resize t)
;;   ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
;;   ;; (setq vertico-cycle t))
;;   )

;; Prescient provides filtering and sorting for completion options.
;; NOTE: Disabled in favor of `orderless` for now.
;; (use-package selectrum-prescient
;;   :ensure t
;;   :init (selectrum-prescient-mode +1)
;;         (prescient-persist-mode +1))

;; Orderless is an alterantive to Prescient. I find it provides a more pleasing visual result.
;; (use-package orderless
;;   :ensure t
;;   :custom
;;   (completion-styles '(orderless basic))
;;   (completion-category-overrides '((file (styles basic partial-completion))))
;;   (orderless-skip-highlighting (lambda () selectrum-is-active))
;;   (selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; Enable richer annotations using the Marginalia package
;; (use-package marginalia
;;   :bind (("M-A" . marginalia-cycle)
;;          :map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))
;;   :init
;;   (marginalia-mode))

;; (use-package embark
;;   :ensure t
;;   :bind
;;   (("C-." . embark-act)         ;; pick some comfortable binding
;;    ("C-;" . embark-dwim)        ;; good alternative: M-.
;;    ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
;;   :init
;;   ;; Optionally replace the key help with a completing-read interface
;;   (setq prefix-help-command #'embark-prefix-help-command)
;;   :config
;;   ;; Hide the mode line of the Embark live/completions buffers
;;   (add-to-list 'display-buffer-alist
;;                '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
;;                  nil
;;                  (window-parameters (mode-line-format . none))))
;;   )

;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t
;;   :after (embark consult)
;;   :demand t ; only necessary if you have the hook below
;;   ;; if you want to have consult previews as you move around an
;;   ;; auto-updating embark collect buffer
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

;;
;; Prespective for buffer management.
;;

;; (use-package perspective
;;   :ensure t
;;   :bind (("C-x C-b" . persp-list-buffers)
;;          ("C-x b" . persp-switch-to-buffer*)
;;          ("C-x k" . persp-kill-buffer*))
;;   :custom (persp-mode-prefix-key (kbd "C-x x"))
;;   :init (persp-mode))

;;
;; Projectile.
;;
;; DEPRECATED IN FAVOUR OF `projects.el`.
;;

;; (use-package projectile
;;   :init
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   (add-to-list 'projectile-globally-ignored-directories "node_modules"))

;;
;; Icons all 'round!
;;

;; (use-package all-the-icons
;;   :ensure t)

;;
;; Sidebar (treemacs).
;;

;; (use-package treemacs
;;   :ensure t
;;   :defer t
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                 (if (treemacs--find-python3) 3 0)
;;           treemacs-deferred-git-apply-delay      0.5
;;           treemacs-display-in-side-window        t
;;           treemacs-eldoc-display                 t
;;           treemacs-file-event-delay              5000
;;           treemacs-file-follow-delay             0.2
;;           treemacs-follow-after-init             t
;;           treemacs-git-command-pipe              ""
;;           treemacs-goto-tag-strategy             'refetch-index
;;           treemacs-indentation                   2
;;           treemacs-indentation-string            " "
;;           treemacs-is-never-other-window         nil
;;           treemacs-max-git-entries               5000
;;           treemacs-missing-project-action        'ask
;;           treemacs-no-png-images                 nil
;;           treemacs-no-delete-other-windows       t
;;           treemacs-project-follow-cleanup        nil
;;           treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                      'left
;;           treemacs-recenter-distance             0.1
;;           treemacs-recenter-after-file-follow    nil
;;           treemacs-recenter-after-tag-follow     nil
;;           treemacs-recenter-after-project-jump   'always
;;           treemacs-recenter-after-project-expand 'on-distance
;;           treemacs-show-cursor                   nil
;;           treemacs-show-hidden-files             t
;;           treemacs-silent-filewatch              nil
;;           treemacs-silent-refresh                nil
;;           treemacs-sorting                       'alphabetic-desc
;;           treemacs-space-between-root-nodes      t
;;           treemacs-tag-follow-cleanup            t
;;           treemacs-tag-follow-delay              1.5
;;           treemacs-width                         35)

;;     ;; The default width and height of the icons is 22 pixels. If you are
;;     ;; using a Hi-DPI display, uncomment this to double the icon size.
;;     ;;(treemacs-resize-icons 44)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null (treemacs--find-python3))))
;;       (`(t . t)
;;        (treemacs-git-mode 'deferred))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

;; (use-package treemacs-magit
;;   :after treemacs magit
;;   :ensure t)

;;
;; Graphql mode
;;

;; (use-package graphql-mode
;;   :ensure t)

;;
;; Docker.
;;

;; ;; dockerfile-mode.
;; (use-package dockerfile-mode
;;   :ensure t
;;   :mode "Dockerfile\\'")

;; ;; docker-compose-mode.
;; (use-package docker-compose-mode
;;   :ensure t)

;; ;; docker.
;; (use-package docker
;;   :ensure t
;;   :bind ("C-c d" . docker))

;;
;; Git.
;;

;; ;; Magit.
;; (use-package magit
;;   :ensure t
;;   :bind (("C-x g" . magit-status)
;;          ("C-x M-g" . magit-dispatch-popup)))

;; ;; Git gutter (diff-hl)
;; (use-package diff-hl
;;   :ensure t
;;   :config
;;   (global-diff-hl-mode 1)
;;   (diff-hl-flydiff-mode 1)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;
;; Groovy
;;

;; (use-package groovy-mode
;;   :ensure t)

;;
;; Haskell
;;

;; (use-package haskell-mode
;;   :ensure t)

;;
;; Terraform.
;;

;; (use-package terraform-mode
;;   :ensure t)

;;
;; LUA.
;;

;; (use-package lua-mode
;;   :ensure t)

;;
;; Line numbers.
;;

;; (add-hook 'conf-mode-hook #'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; (add-hook 'text-mode-hook #'display-line-numbers-mode)
;; (setq-default
;;  display-line-numbers-grow-only t
;;  ;; display-line-numbers-type 'relative
;;  ;; display-line-numbers-width 3
;;  )

;;
;; PlantUML
;;

;; (use-package plantuml-mode
;;   :ensure t
;;   :config (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
;;                 plantuml-default-exec-mode 'jar)
;;   :mode ("\\.plantuml\\'"))

;;
;; OpenSCAD.
;;

;; (use-package scad-mode
;;   :ensure t)

;;
;; Org mode.
;;

;; (use-package org
;;   :elpaca nil
;;   :custom
;;   (org-adapt-indentation nil)
;;   (org-babel-python-command "python")
;;   (org-confirm-babel-evaluate nil)
;;   (org-cycle-separator-lines 0)
;;   (org-descriptive-links nil)
;;   (org-edit-src-content-indentation 0)
;;   (org-edit-src-persistent-message nil)
;;   (org-fontify-done-headline t)
;;   (org-fontify-quote-and-verse-blocks t)
;;   (org-fontify-whole-heading-line t)
;;   (org-return-follows-link t)
;;   (org-src-preserve-indentation t)
;;   (org-src-tab-acts-natively t)
;;   (org-src-window-setup 'current-window)
;;   (org-startup-truncated nil)
;;   (org-support-shift-select 'always)
;;   (org-pretty-entities t)
;;   (org-hide-emphasis-markers t)
;;   (org-startup-with-inline-images t)
;;   (org-startup-indented t)
;;   (org-display-remote-inline-images 'cache)
;;   (org-startup-folded 'showall)
;;   :config
;;   (setq org-agenda-files '("~/Dropbox/org"))
;;   (require 'ob-shell)
;;   (org-babel-do-load-languages
;;    'org-babel-load-languages '((python . t) (shell . t)))
;;   (modify-syntax-entry ?' "'" org-mode-syntax-table)
;;   ;; (advice-add 'org-src--construct-edit-buffer-name :override
;;   ;;   #'me/org-src-buffer-name)
;;   :hook
;;   (org-mode . buffer-face-mode))

;; (use-package org-appear
;;   :ensure t
;;   :hook (org-mode . org-appear-mode))
