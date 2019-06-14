(set-font (make-instance 'xft:font :family "DejaVu Sans Mono" :subfamily "Book" :size 25))

(setf *window-border-style* :thick)
(update-color-map (current-screen))
(setf *colors* '("black" "red" "green" "magenta" "blue" "#00afaf" "cyan" "#586e75" "#93a1a1"))
(update-color-map (current-screen))

(set-fg-color "#839496")

(setf *mode-line-pad-y* 5)
(stumpwm:run-shell-command "xsetroot -cursor_name left_ptr")


(setf *message-window-gravity* :center)
(setf *input-window-gravity* :center)


;;; Colors for `nordless` emacs theme
(set-bg-color "#2E3440") 
(set-win-bg-color "#2E3440") 
(set-unfocus-color "#2E3440") 
(set-focus-color "#2E3440") 
(setf *mode-line-background-color* "#2E3440") 
(setf *mode-line-border-color*     "#2E3440")

;;; Colors for `nofrils` vim theme
;(setf *mode-line-background-color* "#303030") 
;(setf *mode-line-border-color*     "#262626")
;(set-bg-color "#262626") 
