;;
;; Aesthetics.
;;

(setq custom-safe-themes t)

(use-package dracula-theme
  :ensure t
  :config (load-theme 'dracula))

;; Prepare typefaces.
(let ((font-fixed "FantasqueSansM Nerd Font")
      (font-size 140)
      (font-variable "FantasqueSansM Nerd Font"))
  (set-face-attribute 'default nil :font font-fixed :height font-size)
  (set-face-attribute 'fixed-pitch nil :font font-fixed :height font-size)
  (set-face-attribute 'mode-line nil :height font-size :inherit 'default)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
  (set-face-attribute 'variable-pitch nil :font font-variable))

;; Use a different Japanese font.
(if window-system
    (set-fontset-font (frame-parameter nil 'font)
                      'japanese-jisx0208
                      '("VL Gothic" . "unicode-bmp")))

;; Always show fill column indicator.
(global-display-fill-column-indicator-mode t)
(set-face-attribute 'fill-column-indicator nil :foreground "grey30")

;; NOTE: Remember to run `all-the-icons-install-fonts`.
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(provide 'setup-aesthetics)
