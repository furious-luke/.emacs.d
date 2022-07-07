;; I wanted to use Olivetti, but I also really wanted text that overruns the
;; right margin to remain on the same line.
(defun my/focus-mode-on ()
  (let ((margin-size (/ (- (frame-width) fill-column) 2)))
    (set-window-margins nil margin-size nil)))
(defun my/focus-mode-off ()
  (set-window-margins nil nil nil))

(defcustom my/focus-mode-blacklist-buffers '()
  "Buffers for which `my/focus-mode' should not be enabled automatically."
  :type '(repeat string))

(defcustom my/focus-mode-blacklist-modes '(magit-status-mode
                                           minibuffer-mode
                                           minibuffer-inactive-mode
                                           tabulated-list-mode)
  "Modes for which `my/focus-mode' should not be enabled automatically."
  :type '(repeat symbol))

(defun my/focus-mode-maybe--predicate (window)
  (with-selected-window window
    (and (not (window-parameter window 'no-other-window))
         (not (window-parameter window 'window-side))
         (not (member (buffer-name) my/focus-mode-blacklist-buffers))
         (not (apply 'derived-mode-p my/focus-mode-blacklist-modes)))))

(defun my/focus-mode-maybe (&optional frame)
  (let ((windows (seq-filter #'my/focus-mode-maybe--predicate
                             (window-list frame)))
        (columns (frame-total-cols frame)))
    (dolist (window windows)
      (with-selected-window window
        (pcase-let ((`(,l ,_t ,r ,_b) (window-edges window)))
          (if (and (equal l 0) (equal r columns))
              (my/focus-mode-on)
            (my/focus-mode-off)))))))

(defun my/enable-focus-mode ()
  (interactive)
  (add-hook 'window-configuration-change-hook 'my/focus-mode-maybe))
(defun my/disable-focus-mode ()
  (interactive)
  (remove-hook 'window-configuration-change-hook 'my/focus-mode-maybe)
  (my/focus-mode-off))

(my/enable-focus-mode)

(provide 'setup-my-focus)
