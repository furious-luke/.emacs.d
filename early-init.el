(setq-default
 load-prefer-newer t
 ; mode-line-format nil
 package-enable-at-startup nil
 package-native-compile t)

(setq-default
 default-frame-alist
 '((background-color . "#101010")       ; Default background color
   (foreground-color . "#FAFAFA")       ; Default foreground color
   (fullscreen . maximized)             ; Maximize the window by default
   (horizontal-scroll-bars . nil)       ; No horizontal scroll-bars
   ;; (left-fringe . 8)                    ; Thin left fringe
   (menu-bar-lines . 0)                 ; No menu bar
   ;; (right-divider-width . 2)            ; Thin vertical window divider
   ;; (right-fringe . 8)                   ; Thin right fringe
   (tool-bar-lines . 0)                 ; No tool bar
   ;; (undecorated . t)                    ; Remove extraneous X decorations
   (vertical-scroll-bars . nil)))       ; No vertical scroll-bars
