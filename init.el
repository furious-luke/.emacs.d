;;
;; Straight, a package manager for Emacs.
;;

;; Bootstrap Straight.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrate Straight with Package.
(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;;
;; Internal Emacs Lisp settings.
;;

;; Make use of all that RAM we have.
(setq max-specpdl-size 3200
      max-lisp-eval-depth 3200
      gc-cons-threshold 100000000)

 ;; Lock files will kill `npm start'.
(setq create-lockfiles nil)

;; Show me debugging output for all my mistakes.
;; NOTE: This breaks lsp-install-server
;; (setq debug-on-error t)

;; I've always hated trying to reload the init file.
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "<f5>") 'reload-init-file)

;; Don't litter with autosave/backups.
(setq make-backup-files nil)
(setq auto-save-default nil)

;;
;; Help me find the command I want.
;;

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;
;; MacOS corrections.
;;

;; Environment variables don't make it into Emacs in MacOS.
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;; MacOS uses a different `ls`.
(if (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil))

;;
;; Basic editing.
;;

(setq-default indent-tabs-mode nil)

;;
;; Environment inclusion.
;;

;; Respect .envrc files.
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;
;; Aesthetics.
;;

;; Remove menu/tool bars.
(if window-system (menu-bar-mode -1))
(if window-system (tool-bar-mode -1))

;; Use Modus Vivendi theme. I prefer the colors of Dracula, however
;; Modus Vivendi has much better contrast.
(setq custom-safe-themes t)
(use-package modus-themes
  :ensure t)
(load-theme 'modus-vivendi)

;; Different font size between Linux and Darwin.
(setq bmw/face-height-default
      (if (eq system-type 'darwin)
          180
        140))

;; Set default font to Fantasque Sans Mono.
(set-face-attribute 'default nil
                    :family "FantasqueSansMono Nerd Font"
                    :height bmw/face-height-default)

;; Manage HiDPI environments.
(defun my-dpi (&optional frame)
  "Get the DPI of FRAME (or current if nil)."
  (cl-flet ((pyth (lambda (w h)
                    (sqrt (+ (* w w)
                             (* h h)))))
            (mm2in (lambda (mm)
                     (/ mm 25.4))))
    (let* ((atts (frame-monitor-attributes frame))
           (pix-w (cl-fourth (assoc 'geometry atts)))
           (pix-h (cl-fifth (assoc 'geometry atts)))
           (pix-d (pyth pix-w pix-h))
           (mm-w (cl-second (assoc 'mm-size atts)))
           (mm-h (cl-third (assoc 'mm-size atts)))
           (mm-d (pyth mm-w mm-h)))
      (/ pix-d (mm2in mm-d)))))
(defvar my-zoom-frm-wanted-dpi 70
  "The DPI I want to achieve when using `my-zoom-frm-by-dpi'.")
(defun my-zoom-frm-by-dpi (&optional frame)
  "Zoom FRAME so the DPI is closer to `my-zoom-frm-wanted-dpi'."
  (interactive)
  (let ((frame (or frame (selected-frame))))
    (when (frame-parameter frame 'zoomed)
      (zoom-frm-unzoom frame))
    (let ((frame-zoom-font-difference (1- (round (/ (my-dpi frame)
                                                    my-zoom-frm-wanted-dpi)))))
      (when (called-interactively-p 'interactive)
        (message "Zooming by %S" frame-zoom-font-difference))
      (zoom-frm-in frame))))
(add-hook 'after-make-frame-functions #'my-zoom-frm-by-dpi)  ;; apply the scaling I want to each newly created frame

;;
;; Recent files and history.
;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(use-package savehist
  :ensure t
  :init (savehist-mode))

;;
;; Completion framework.
;;

(use-package company
  :ensure t
  :init (setq company-idle-delay 0.3
              company-minimum-prefix-length 1))

;;
;; Linting with Flycheck.
;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;;
;; Language Server Protocol.
;;

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((prog-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :ensure t)

(use-package lsp-treemacs
  :after lsp-mode
  :ensure t)

(use-package lsp-pyright
  :after lsp-mode
  :ensure t)

;;
;; JavaScript/TypeScript.
;;

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :straight (:type git :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;;
;; Bookmarks and annotations.
;;

;; Enable annotations for bookmarks. NOTE: This is not needed unless
;; we want the annotation to be forced when creating bookmarks.
;; (setq bookmark-use-annotations t)

;; Integrate with eshell. TODO: Do I need this anymore?
(use-package eshell-bookmark
  :ensure t
  :config
  (add-hook 'eshell-mode-hook 'eshell-bookmark-setup))

;;
;; Completion system.
;;

(use-package selectrum
  :ensure t
  :init
  (selectrum-mode +1))

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
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-skip-highlighting (lambda () selectrum-is-active))
  (selectrum-highlight-candidates-function #'orderless-highlight-matches))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Lots and lots of great utilities.
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;
;; Prespective for buffer management.
;;

(use-package perspective
  :ensure t
  :bind (("C-x C-b" . persp-list-buffers)
         ("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*))
  :custom (persp-mode-prefix-key (kbd "C-x x"))
  :init (persp-mode))

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

(use-package all-the-icons
  :ensure t)

;;
;; Sidebar (treemacs).
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

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

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

;;
;; Graphql mode
;;

(use-package graphql-mode
  :ensure t)

;;
;; Docker.
;;

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

;;
;; Git.
;;

;; Magit.
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; Git gutter (diff-hl)
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;;
;; Groovy
;;

(use-package groovy-mode
  :ensure t)

;;
;; Haskell
;;

(use-package haskell-mode
  :ensure t)

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
;; Multiple cursors.
;;

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
  :init (setq markdown-command "multimarkdown")
        (setq markdown-fontify-code-blocks-natively t))

;;
;; PlantUML
;;

(use-package plantuml-mode
  :ensure t
  :config (setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
                plantuml-default-exec-mode 'jar)
  :mode ("\\.plantuml\\'"))

;;
;; OpenSCAD.
;;

(use-package scad-mode
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
