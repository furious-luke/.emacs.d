;;
;; Sensible defaults.
;;

(setq-default
 ad-redefinition-action 'accept         ; Silence warnings for redefinition
 auto-save-list-file-prefix nil         ; Prevent tracking for auto-saves
 cursor-in-non-selected-windows nil     ; Hide the cursor in inactive windows
 ;; cursor-type '(hbar . 2)                ; Underline-shaped cursor
 custom-unlispify-menu-entries nil      ; Prefer kebab-case for titles
 custom-unlispify-tag-names nil         ; Prefer kebab-case for symbols
 delete-by-moving-to-trash t            ; Delete files to trash
 fill-column 120                        ; Set width for automatic line breaks
 gc-cons-threshold (* 8 1024 1024)      ; We're not using Game Boys anymore
 help-window-select t                   ; Focus new help windows when opened
 indent-tabs-mode nil                   ; Stop using tabs to indent
 inhibit-startup-screen t               ; Disable start-up screen
 initial-scratch-message ""             ; Empty the initial *scratch* buffer
 mouse-yank-at-point t                  ; Yank at point rather than pointer
 native-comp-async-report-warnings-errors 'silent ; Skip error buffers
 read-process-output-max (* 1024 1024)  ; Increase read size for data chunks
 scroll-conservatively 101              ; Avoid recentering when scrolling far
 scroll-margin 2                        ; Add a margin when scrolling vertically
 select-enable-clipboard t              ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil          ; Use a single space after dots
 show-help-function nil                 ; Disable help text everywhere
 tab-always-indent 'complete            ; Indent first then try completions
 tab-width 4                            ; Smaller width for tab characters
 uniquify-buffer-name-style 'forward    ; Uniquify buffer names
 use-short-answers t                    ; Replace yes/no prompts with y/n
 window-combination-resize t            ; Resize windows proportionally
 x-stretch-cursor t                     ; Stretch cursor to the glyph width
 create-lockfiles nil                   ; Lockfiles break `npm start`
 make-backup-files nil                  ; Don't save backup files
 auto-save-default nil)                 ; Don't save backup files
;; (blink-cursor-mode 0)                   ; Prefer a still cursor
(delete-selection-mode 1)               ; Replace region when inserting text
(global-subword-mode 1)                 ; Iterate through CamelCase words
(mouse-avoidance-mode 'exile)           ; Avoid collision of mouse with point
(put 'downcase-region 'disabled nil)    ; Enable `downcase-region'
(put 'upcase-region 'disabled nil)      ; Enable `upcase-region'
(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding

;; (setq debug-on-error t) ; Enable debugging

(put 'add-function 'lisp-indent-function 2)
(put 'advice-add 'lisp-indent-function 2)
(put 'plist-put 'lisp-indent-function 2)

(global-unset-key (kbd "C-x C-z"))

;; Garbage collect on focus-out.
(add-function :after after-focus-change-function
  (defun me/garbage-collect-maybe ()
    (unless (frame-focus-state)
      (garbage-collect))))

;; Disable addition of customisations to this file.
(setq-default custom-file null-device)

;; I've always hated trying to reload the init file.
(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "<f5>") 'reload-init-file)

;;
;; Fullscreen immediately.
;;

(pcase window-system
  ('w32 (set-frame-parameter nil 'fullscreen 'fullboth))
  (_ (set-frame-parameter nil 'fullscreen 'maximized)))

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
  :if (eq window-system 'ns)
  :ensure t
  :init (exec-path-from-shell-initialize))

;; MacOS uses a different `ls`.
(if (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil))

;;
;; Environment inclusion.
;;

;; Respect .envrc files.
(use-package direnv
  :ensure t
  :config
  (direnv-mode))

;;
;; Recent files and history.
;;

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(use-package savehist
  :elpaca nil
  :init (savehist-mode))

;;
;; Line numbers
;;

(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(setq-default
 display-line-numbers-grow-only t
 ;; display-line-numbers-type 'relative
 ;; display-line-numbers-width 3
 )

(provide 'setup-general)
