;;; init --- My Emacs config
;;; Commentary:
;;; Code:

;; Variables used in init but defined elsewhere
(eval-when-compile
  (defvar eshell-destroy-buffer-when-process-dies)
  (defvar eshell-history-file-name)
  (defvar eshell-history-ring)
  (defvar eshell-hist-ignoredups)
  (defvar eshell-history-size)
  (defvar eshell-visual-commands)
  (defvar eshell-visual-subcommands)
  (defvar file-name-handler-alist-original)
  (defvar gc-cons-threshold-original)
  (defvar ivy-display-functions-alist)
  (defvar js-indent-level)
  (defvar js2-basic-offset)
  (defvar mouse-off-time)
  (defvar mouse-enabled)
  (defvar mouse-was-off-for-duration)
  (defvar org-agenda-files)
  (defvar org-agenda-window-setup)
  (defvar org-capture-templates)
  (defvar server-buffer-clients)
  (defvar xterm-color-preserve-properties))

;;;;;; Set garbage collection threshold
;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 3000))
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq tab-width 2)

 (require 'package)
 (package-initialize)
;; ;; This is only needed once, near the top of the file
 (eval-when-compile
 (require 'use-package))
 (require 'general)

 (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
 (use-package company-lsp)
(push 'company-lsp company-backends)
;; Because we're using evil, we can navigate completions with C-n/C-p
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))
(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)
; keep all my backups together and out of the way
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq echo-keystrokes .01)
(add-hook 'after-init-hook 'global-company-mode)

(use-package flycheck)
(use-package rust-mode
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook 'flycheck-mode))
(use-package lsp-mode
  :commands lsp
  :init)
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package company-lsp :commands company-lsp)

(setq evil-want-C-i-jump nil)
(use-package evil
  :init
  (evil-mode 1))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(ivy-mode 1)



;; Smartparens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :commands (smartparens-mode smartparens-strict-mode)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))
(add-hook 'js-mode-hook #'smartparens-mode)
(use-package smartparens-config
  :ensure smartparens
  :config (progn (show-smartparens-global-mode t)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)



(fset 'yes-or-no-p 'y-or-n-p)

;; spelling
(use-package flyspell
  :init
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (setq flyspell-issue-message-flag nil)
    (setq ispell-grep-command "rg")
    (defun spell-good ()
      "Add the word under the point to the spelling dictionary."
      (interactive)
      (let ((current-location (point))
      (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct
       'save nil
       (car word)
       current-location
       (cadr word)
       (caddr word)
       current-location))))

    (define-key evil-normal-state-map (kbd "z g") 'spell-good)
    (define-key evil-normal-state-map (kbd "z r") 'flyspell-auto-correct-word))

;; Markdown command
(setq markdown-command "/usr/bin/pandoc")

;; Rebinding suggested by Steve Yegge and non-conflicting
(global-set-key "\C-x\C-m" 'execute-extended-command)

(global-set-key "\C-x\[" 'next-buffer)
(global-set-key "\C-x\]" 'previous-buffer)

;; Toggle lsp popups
(global-set-key "\C-x\p" 'lsp-ui-mode)
;; TODO: figure out why toggling this off manually is required for flycheck errors

(when (fboundp 'winner-mode)
    (winner-mode 1))
;;(global-set-key "\C-wt" 'winner-undo)

;; C-x k should kill current buffer regardless of whether it's nested
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

  (global-set-key "\C-x\k" 'kill-this-buffer)



;; Move-text
;;   NOTE: I would like to get this working with evil-mode's visual selection and
;;         then use C-k/C-j bindings.  But evil-mode does something fancy with
;;         visual selection that I haven't dug into yet.
(general-define-key
  "M-k" 'move-text-up)
(general-define-key
  "M-j" 'move-text-down)

;; Visual
(load-theme 'nordless t)
(setq frame-title-format "Emacs")
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono"))
;; testing
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq fill-column 79)
;;  The following lines are oddly slow to execute, so scroll/menu/tool bars
;;  are removed in the .Xresources file, which is faster (why?)
; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(add-hook 'term-mode-hook #'eterm-256color-mode)

(require 'multi-term)
(setq multi-term-program "/usr/bin/fish")
(defalias 'st 'multi-term)
(setq multi-term-dedicated-select-after-open-p t)
(define-key term-mode-map (kbd "C-j") '(lambda ()
                                         (interactive)
                                         (term-char-mode)
                                         (setq minibuffer-line-format "-- CHAR --")))
(define-key term-raw-map (kbd "C-j") '(lambda ()
                                        (interactive)
                                        (term-line-mode)
                                        (setq minibuffer-line-format "-- LINE --")))
(define-key term-mode-map (kbd "RET") 'term-send-input)
(define-key term-mode-map (kbd "<C-return>") 'evil-ret)

(setq-default mode-line-format nil)
(require 'minibuffer-line)

(run-with-idle-timer .1 t '(lambda () (setq minibuffer-line-format
  (concat
   "%b"
   (make-string (left-padding) ? )
   "%p"
   (make-string (right-padding) ? )
   "(%l,%c)"))))

(setq minibuffer-line-refresh-interval .1)
(minibuffer-line-mode)

(defun left-padding ()
  "Set the padding between the left of the minibuffer line and the middle."
  (- (/ (frame-width) 2)
    (length (format-mode-line "%b"))
    (/ (length (format-mode-line "%p")) 2)))
(defun right-padding  ()
  "Set the padding between the middle of the minibuffer line and the right."
  (- (/ (frame-width) 2)
    (length (format-mode-line "(%l,%c)"))
    (/ (length (format-mode-line "%p")) 2)
    1))


(global-undo-tree-mode)
(setq undo-tree-auto-save-history 1)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-history")))

(setq flycheck-display-errors-delay 0.6)

;; See which keys I can press next.  Maybe disable once I've learned?
(require 'which-key)
(which-key-mode)

;; Track my most-frequently used keys.  Maybe I should remap them?
;; (show stats with M-x keyfreq show
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;; Modify fringe marker for error/warnings to support HDPI.
;; (Hopefully remove this once Flycheck merges PR.)
(fringe-mode 16)

(define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
  (vector #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b1100001111000000
          #b1100001111000000
          #b0011110011110000
          #b0011110011110000
          #b0000111100111100
          #b0000111100111100
          #b0000001111001111
          #b0000001111001111
          #b0000111100111100
          #b0000111100111100
          #b0011110011110000
          #b0011110011110000
          #b1100001111000000
          #b1100001111000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000
          #b0000000000000000) 32 16)
(require 'evil-tabs)
(global-evil-tabs-mode t)
(setq elscreen-display-tab nil)
;; TODO: figure out how to save global marks to disk.
;;; tested above
;;;; 
;;;;  (column-number-mode 1)
;;;;  
;;;;  ;; set key for agenda
;;;;  (global-set-key (kbd "C-c a") 'org-agenda)
;;;;  
;;;;  ;;file to save todo items
;;;;  (setq org-agenda-files (quote ("~/org")))
;;;;  
;;;;  ;;open agenda in current window
;;;;  (setq org-agenda-window-setup (quote current-window))
;;;;  
;;;;  ;;capture todo items using C-c c t
;;;;  (define-key global-map (kbd "C-c c") 'org-capture)
;;;;  (setq org-capture-templates
;;;;        '(("t" "todo" entry (file+headline "~/org/main_todo.org" "Tasks")
;;;;           "* TODO %?")))
;;;;      
;;;;  ;; More helpful help
;;;;  
;;;;  ;; Note that the built-in `describe-function' includes both functions
;;;;  ;; and macros. `helpful-function' is functions only, so use `helpful-callable'.
;;;;  (global-set-key (kbd "C-h f") #'helpful-callable)
;;;;  (global-set-key (kbd "C-h v") #'helpful-variable)
;;;;  (global-set-key (kbd "C-h k") #'helpful-key)
;;;;  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
;;;;  ;; for this in lisp modes.
;;;;  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
;;;;  ;; Look up Functions (excludes macros).
;;;;  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;;;;  ;; already links to the manual, if a function is referenced there.
;;;;  (global-set-key (kbd "C-h F") #'helpful-function)
;;;;  ;; Look up Commands.
;;;;  ;; By default, C-h C is bound to describe `describe-coding-system'. I
;;;;  ;; don't find this very useful, but it's frequently useful to only
;;;;  ;; look at interactive functions.
;;;;  (global-set-key (kbd "C-h C") #'helpful-command)
;;;;  
;;;;  
;;;;  ;; Prettier
;;;;  (require 'prettier-js)
;;;;  ;(add-hook 'js2-mode-hook 'prettier-js-mode)
;;;;  ;(add-hook 'web-mode-hook 'prettier-js-mode)
;;;;  
;;;;  ;; Projectile
;;;;  (require 'projectile)
;;;;  (setq projectile-completion-system 'ivy)
;;;;  ;; (global-set-key "\C-c\p" 'projectile-find-file-dwim)


;;; tested below
;; eshell
(require 'esh-help)
(setup-esh-help-eldoc)  ;; To use eldoc in Eshell
(require 'eshell)
(require 'em-smart)
(require 'eshell-up)
(require 'eshell-z)
(require 'em-tramp) ; to load eshell’s sudo
(add-to-list 'eshell-modules-list 'eshell-tramp)
(setq tramp-default-method "ssh")
(setq password-cache t) ; enable password caching
(setq password-cache-expiry (* 5 60))
(setq eshell-where-to-jump 'begin)
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-review-quick-commands t)
(setq eshell-smart-space-goes-to-end t)
(setq eshell-needs-pipe '("bc" "xclip"))
(add-hook 'eshell-mode-hook 'eshell-smart-initialize)
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)
(when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode))
 (add-hook 'eshell-mode-hook
          (lambda ()
            (add-to-list 'eshell-visual-commands "htop")
            (add-to-list 'eshell-visual-subcommands '("cdt" "stats"))
            (add-to-list 'eshell-visual-commands "wifi-on")
            (add-to-list 'eshell-needs-pipe "xclip")))
 (add-hook 'eshell-mode-hook
          (lambda ()
            (setenv "TERM" "eterm")
            (setenv "PAGER" "")
            (setenv "EDITOR" "emacsclient")
            (setenv "VISUAL" "emacsclient")
            (setq eshell-path-env (concat "/home/dsock/.cargo/bin/:" eshell-path-env))
            ))
(add-hook 'eshell-before-prompt-hook (lambda () (setq xterm-color-preserve-properties t)))
(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions
      (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
; (setq xterm-color-debug t)
 (defun esh ()
  "Open a new eshell instance."
  (interactive)
  (eshell 'N))
 (defun setup-eshell-ivy-completion ()
  "Use ivy for eshell completion rather than a separate buffer."
  (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
  ;; only if you want to use the minibuffer for completions instead of the
  ;; in-buffer interface
  (setq-local ivy-display-functions-alist
     (remq (assoc 'ivy-completion-in-region ivy-display-functions-alist)
           ivy-display-functions-alist)))
 (add-hook 'eshell-mode-hook #'setup-eshell-ivy-completion)
(defun mu-ivy-eshell-history ()
  "Browse Eshell history with Ivy."
  (interactive)
  (insert
    (ivy-read "Eshell history: "
              (delete-dups
              (ring-elements eshell-history-ring)))))
 (defun edit-prev-cmd ()
  "Prevent the cursor from jumping down to the next line once the user has entered command mode."
  (interactive)
  (remove-hook 'pre-command-hook 'eshell-smart-display-move t))
 (add-hook 'eshell-mode-hook
          (lambda ()
          (general-define-key
            :states 'insert
            :keymaps 'eshell-mode-map
            "C-p" 'eshell-previous-matching-input-from-input)
          (general-define-key
            :states 'normal
            :keymaps 'eshell-mode-map
            "C-p" 'eshell-previous-prompt)
          (general-define-key
            :states 'insert
            :keymaps 'eshell-mode-map
            "C-n" 'eshell-next-matching-input-from-input)
          (general-define-key
            :states 'insert
            :keymaps 'eshell-mode-map
            "C-S-v" 'yank)
          (general-define-key
            :states 'normal
            :keymaps 'eshell-mode-map
            "C-n" 'eshell-next-prompt)
          (general-define-key
            :states '(normal insert)
            :keymaps 'eshell-mode-map
            "C-r" 'mu-ivy-eshell-history)
          (general-define-key
           :states 'normal
           :keymaps 'eshell-mode-map
           "I" 'insert-after-prompt)
          (general-define-key
           :states 'normal
           :keymaps 'eshell-mode-map
           "^" 'first-non-blank-after-prompt)
          (general-define-key
           :states 'normal
           :keymaps 'eshell-mode-map
           "^" 'eshell-bol)
          (general-define-key
           :states 'normal
           :keymaps 'eshell-mode-map
           "G" 'goto-line-after-prompt)
          (bind-key (kbd "<C-return>") 'evil-ret eshell-mode-map)
          (add-hook 'evil-normal-state-entry-hook 'edit-prev-cmd eshell-mode-map)))
 (defun first-non-blank-after-prompt ()
  "Places cursor at the beginning of the line, right after the prompt."
  (interactive)
  (eshell-bol))
 (defun insert-after-prompt ()
  "Enters insert state immediately after the eshell prompt."
  (interactive)
  (eshell-bol)
  (evil-insert 1))
 (defun delete-line-after-prompt ()
  "Deletes the line after the prompt."
  (interactive)
  (eshell-bol)
  (evil-delete-line nil nil))
 (defun goto-line-after-prompt (&optional count)
  "Go to immediately after the eshell prompt of line COUNT.
By default, the last line."
  (interactive)
  (evil-goto-line count)
  (eshell-bol))

;;; Shared history.
(defvar eshell-history-global-ring nil
  "The history ring shared across Eshell sessions.")
 (setq eshell-history-size 8192)
(setq eshell-hist-ignoredups t)
 (defun eshell-hist-use-global-history ()
  "Make Eshell history shared across different sessions."
  (unless eshell-history-global-ring
    (let (eshell-history-ring)
      (when eshell-history-file-name
        (eshell-read-history nil t))
      (setq eshell-history-global-ring eshell-history-ring))
    (unless eshell-history-ring
      (setq eshell-history-global-ring (make-ring eshell-history-size))))
  (setq eshell-history-ring eshell-history-global-ring))
 (add-hook 'eshell-mode-hook 'eshell-hist-use-global-history)
;; Add syntax highlighting to cat
;; https://github.com/manateelazycat/aweshell
(defun aweshell-cat-with-syntax-highlight (filename)
  "Run cat(1) on FILENAME, but displayed with syntax highlighting from the current theme."
  (let ((existing-buffer (get-file-buffer filename))
	(buffer (find-file-noselect filename)))
    (eshell-print
     (with-current-buffer buffer
       (if (fboundp 'font-lock-ensure)
	   (font-lock-ensure)
	 (with-no-warnings
	   (font-lock-fontify-buffer)))
       (buffer-string)))
    (unless existing-buffer
      (kill-buffer buffer))
    nil))
 (advice-add 'eshell/cat :override #'aweshell-cat-with-syntax-highlight)
 (run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold gc-cons-threshold-original)
   (setq file-name-handler-alist file-name-handler-alist-original)
   (makunbound 'gc-cons-threshold-original)
   (makunbound 'file-name-handler-alist-original)
   (message "gc-cons-threshold and file-name-handler-alist restored")))
;;; Scripts
 (setq mouse-enabled
        (if (=
              (shell-command "xinput list 'SynPS/2 Synaptics TouchPad' | grep -c 'disabled'")
              ;; This is checking the *exit code*, not the count (so 0 = "found"; 1 = "not found"
             1)
        t
        nil))
 (defun toggle-mouse ()
  "Toggle the mouse on or off.
 If toggling it on, report how long the user successfully kept it off."
  (interactive)
  (if mouse-enabled
      (progn
        (setq mouse-enabled nil)
        (setq mouse-off-time (format-time-string "%s"))
        (write-region mouse-off-time nil "~/.emacs.d/data/mousetime")
        (shell-command
         "xinput --disable 'SynPS/2 Synaptics TouchPad'")
        (message "mouse disabled"))
    (progn
      (setq mouse-enabled t)
      (setq mouse-off-time (get-string-from-file "~/.emacs.d/data/mousetime"))
      (setq mouse-was-off-for-duration (-
                        (string-to-number
                         (format-time-string "%s"))
                        (string-to-number mouse-off-time)))
      (shell-command
       "xinput --enable 'SynPS/2 Synaptics TouchPad'")
      (message (concat
       "mouse enabled. You kept it off for "
       (seconds-to-string mouse-was-off-for-duration))))))
 (defun get-string-from-file (filePath)
  "Return FILEPATH's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))
 ;; Use pipes to communicate with spawned processes rather than TTYs
;; This solves a number of issues and makes things a lot faster to boot:
;;
;; + eshell pipes work without issue
;; + eshell runs a lot faster
;; + shell runs a lot faster but we lose job control
 ;; (setq process-connection-type nil)
 ;; ;; shell advice so that it executes inside a let-binding that
;; ;; sets process-connection-type to t since running shells without
;; ;; ttys is not very useful.
;; (defun xristos/advice-shell (f &rest args)
;;   (let ((process-connection-type t))
;;     (apply f args)))
 ;; (advice-add 'eshell :around #'xristos/advice-shell)
;; (advice-add 'shell :around #'xristos/advice-shell)
;;; init ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (erlang julia-mode julia-repl evil-replace-with-register emojify rust-playground nodejs-repl gnu-apl-mode zig-mode wucuo which-key use-package tco rich-minority racket-mode projectile prettier-js pollen-mode nordless-theme multi-term move-text minibuffer-line lsp-ui lsp-rust keyfreq ivy helpful general flycheck fish-completion exec-path-from-shell evil-tabs evil-surround evil-smartparens eterm-256color esup eshell-z eshell-up esh-help esh-autosuggest diminish company-lsp cargo)))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
