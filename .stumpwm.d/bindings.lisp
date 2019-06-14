(stumpwm:set-prefix-key (stumpwm:kbd "C-SPC"))

;; Remove all default mappings
(setf stumpwm:*root-map*              (stumpwm:make-sparse-keymap))
(setf stumpwm::*float-group-root-map* (stumpwm:make-sparse-keymap))
(setf stumpwm::*tile-group-root-map*  (stumpwm:make-sparse-keymap))
(setf stumpwm::*group-root-map*       (stumpwm:make-sparse-keymap))
(setf stumpwm::*groups-map*           (stumpwm:make-sparse-keymap))

;; Root map
(define-key *root-map* (kbd "i")   "show-window-properties")
(define-key *root-map* (kbd "p")   "pass-copy")
;; Temporarily disabled as finger drill
;; (define-key *root-map* (kbd "C-6") "other-window")
(define-key *root-map* (kbd "C-x") 'buffers)
(define-key *root-map* (kbd "M-x") "run-stumpwm-command" )
(define-key *root-map* (kbd "C-g") "abort-stumpwm-command")
(define-key *root-map* (kbd "C-w") 'wincmd)
(define-key *root-map* (kbd "C-c") 'user-bindings)
(define-key *root-map* (kbd "g")   '*groups-map*)
(define-key *root-map* (kbd "C-h") '*help-map*)
(define-key *root-map* (kbd "'")   '*jump-map*)
(define-key *root-map* (kbd "C-u") '*prefix*)
(define-key *root-map* (kbd "C-SPC") "send-escape")


;; Group map
(define-key *groups-map* (kbd "t")   "tabnext")
(define-key *groups-map* (kbd "T")   "tabprev")
(define-key *groups-map* (kbd "m")   "gmove-and-follow")
(define-key *groups-map* (kbd "M")   "gmove")

;; user-bindings
(defvar user-bindings
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "c")   "org-capture")
    (define-key new-keymap (kbd "a")   "org-agenda")
    new-keymap))
;; buffers map
(defvar buffers
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "b")   "global-pull-windowlist")
    (define-key new-keymap (kbd "0")   "remove-split")
    (define-key new-keymap (kbd "k")   "delete-window")
    (define-key new-keymap (kbd "]")   "next-window-in-group")
    (define-key new-keymap (kbd "[")   "prev-window-in-group")
    (define-key new-keymap (kbd "C-f") "run-shell-command")
    (define-key new-keymap (kbd "C-m") "run-stumpwm-command")
    new-keymap))
;; pbuffers map
(defvar pbuffers
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "b")   "global-windowlist")
    (define-key new-keymap (kbd "k")   "kill-window")
    (define-key new-keymap (kbd "]")   "next-in-frame")
    (define-key new-keymap (kbd "[")   "prev-in-frame")
    (define-key new-keymap (kbd "C-m") "evaluate-lisp-expression")
    new-keymap))

;; prefix map
(defvar *prefix*
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "C-x") 'pbuffers)
    new-keymap))

;; Jump map
(defvar *jump-map*
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "C") "chrome")
    (define-key new-keymap (kbd "E") "emacs")
    (define-key new-keymap (kbd "F") "firefox")
    (define-key new-keymap (kbd "Q") "qutebrowser")
    (define-key new-keymap (kbd "M") "mastodon")
    (define-key new-keymap (kbd "T") "terminal")
    (define-key new-keymap (kbd "R") "ripcord")
    (define-key new-keymap (kbd "O") "org-time")
    new-keymap))

;; wincmd
(defvar wincmd
  (let ((new-keymap (make-sparse-keymap)))
    (define-key new-keymap (kbd "t")   "tiled")
    (define-key new-keymap (kbd "p")   "place")
    (define-key new-keymap (kbd "m")   "monocle")
    (define-key new-keymap (kbd "s")   "horizontal-split")
    (define-key new-keymap (kbd "v")   "vertical-split")
    (define-key new-keymap (kbd "r")   "resize-current-frame")
    (define-key new-keymap (kbd "w")   "fnext")
    (define-key new-keymap (kbd "C-w") "fnext")
    (define-key new-keymap (kbd "q")   "remove")
    (define-key new-keymap (kbd "H")   "swap-window left")
    (define-key new-keymap (kbd "J")   "swap-window down")
    (define-key new-keymap (kbd "K")   "swap-window up")
    (define-key new-keymap (kbd "L")   "swap-window right")
    (define-key new-keymap (kbd "h")   "move-focus left")
    (define-key new-keymap (kbd "j")   "move-focus down")
    (define-key new-keymap (kbd "k")   "move-focus up")
    (define-key new-keymap (kbd "l")   "move-focus right")
    new-keymap))
