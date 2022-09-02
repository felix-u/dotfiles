;; Garbage collection settings for faster startup
(set 'gc-cons-threshold (* 100 1000 1000))

;; Set user directory
(setq user-emacs-directory "~/.emacs.d/")

;; Use <esc> to exit popups and such
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Only show minibuffer for errors, not warnings
(setq warning-minimum-level :error)

;; Disable tooltips
(tooltip-mode -1)

;; Disable the menubar
(menu-bar-mode -1)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Save all tempfiles in ~/.emacs.d/backups
(setq backup-directory-alist
    `(("." . ,(concat user-emacs-directory "backups"))))

;; ;; Uncomment this to show line numbers by default
;; (global-display-line-numbers-mode 1)
;; (setq display-line-numbers-type 'relative)

;; Show columns as well as current line
(setq column-number-mode t)

;; Don't ask about symlinks, just edit the file pointed to
(setq vc-follow-symlinks t)

;; Automatic syntax pairs
(electric-pair-mode 1)

;; Save command history
(savehist-mode 1)
(setq history-length 25)

;; Save file history
(recentf-mode 1)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)


;; Bootstrap straight.el

(unless (featurep 'straight)
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
        (load bootstrap-file nil 'nomessage)))

;; Initialise package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

;; Initialise use-package
(straight-use-package 'use-package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-package-update
    :config
    (setq auto-package-update-delete-old-versions t)
    (setq auto-package-update-hide-results t)
    (auto-package-update-maybe))


;; MWAHAHAHAHAHAHAHA

(use-package evil
    :config
    (evil-mode 1)

    ;; keep selection when indenting
    (defun my/evil-shift-right ()
        (interactive)
        (evil-shift-right evil-visual-beginning evil-visual-end)
        (evil-normal-state)
        (evil-visual-restore))
    (defun my/evil-shift-left ()
        (interactive)
        (evil-shift-left evil-visual-beginning evil-visual-end)
        (evil-normal-state)
        (evil-visual-restore))
    (evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
    (evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)
    (evil-define-key 'insert 'global (kbd "C-<tab>") 'insert-tab-char)

    ;; delete single character without yanking
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char)

    ;; window shortcuts (more below)
    (global-set-key (kbd "<C-return>") 'evil-window-new)
    (global-set-key (kbd "<C-next>") 'evil-window-split)
    (global-set-key (kbd "<C-end>") 'evil-window-vsplit)

    (use-package evil-commentary
        :config (evil-commentary-mode))
    (use-package evil-surround
        :config (global-evil-surround-mode 1))
    (use-package avy ; like lightspeed.nvim
        :config
        (define-key evil-normal-state-map "S" 'avy-goto-char-2-above)
        (define-key evil-normal-state-map "s" 'avy-goto-char-2-below))
    (use-package general
        :config
        (general-evil-setup t)
        (general-create-definer rune/leader-keys
            :keymaps '(normal insert visual emacs)
            :prefix "SPC"
            :global-prefix "C-SPC")
        )

    ;; used in keymap below
    (defun kill-other-buffers ()
        "Kill all other buffers."
        (interactive)
        (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

    (rune/leader-keys

        ;; BASE
        "<SPC>" '(find-file :which-key "find file")
        "." 	'(dired :which-key "browse files (dired)")
        ","     '(consult-buffer :which-key "switch buffer")

        ;; BUFFER
        "b"  '(:ignore t :which-key "buffer")
        "bj" '(consult-buffer :which-key "switch buffer")
        "bi" '(ibuffer :which-key "ibuffer")
        "bk" '(kill-buffer :which-key "kill buffer")
        "bo" '(kill-other-buffers :which-key "kill other buffers")
        "bs" '(save-buffer :which-key "save buffer")
        "b[" '(previous-buffer :which-key "prev buffer")
        "b]" '(next-buffer :which-key "next buffer")

        ;; CODE
        "c"  '(:ignore t :which-key "code")
        "ce" '(eval-buffer :which-key "evaluate buffer")
        "ci" '(consult-imenu :which-key "imenu")
        "cl" '(consult-goto-line :which-key "goto line")

        ;; DESCRIBE
        "d"  '(:ignore t :which-key "describe")
        "db" '(describe-bindings :which-key "bindings")
        "df" '(describe-face :which-key "face")
        "dk" '(describe-key :which-key "key")
        "dm" '(describe-mode :which-key "mode")
        "dn" '(describe-function :which-key "function")
        "dv" '(describe-variable :which-key "variable")

        ;; EMBARK
        "e"  '(:ignore t :which-key "embark")
        "ea" '(embark-act :which-key "act")
        "ed" '(embark-dwim :which-key "dwim")

        ;; FIND
        "f"  '(:ignore t :which-key "file")
        "f." '(find-file :which-key "find file")
        "fr" '(consult-recent-file :which-key "recent files")
        "fl" '(consult-line :which-key "line")
        "fm" '(consult-line-multi :which-key "line-multi")

        ;; LSP
        "l"  '(:ignore t :which-key "lsp")
        "lr" '(iedit-mode :which-key "rename object (iedit)")
        "lf" '(flycheck-list-errors :which-key "flycheck errors")

        ;; OPEN
        "o"  '(:ignore t :which-key "open")
        "ot" '(vterm-toggle :which-key "vterm")

        ;; PROJECTILE
        "p"  '(:ignore t :which-key "projectile")
        "pf" '(projectile-find-file-dwim :which-key "find file")
        "p." '(projectile-dired :which-key "find file")
        "pd" '(projectile-find-dir :which-key "find dir")
        "pa" '(projectile-add-known-project :which-key "add")
        "ps" '(projectile-switch-project :which-key "switch")

        ;; (COLOUR) SCHEME
        "s"  '(:ignore t :which-key "colour scheme")
        "sl" '((lambda () (interactive) (load-theme 'doom-solarized-light t)) :which-key "light theme")
        "sd" '((lambda () (interactive) (load-theme 'doom-solarized-dark t)) :which-key "dark theme")

        ;; TOGGLE
        "t"  '(:ignore t :which-key "toggle")
        "tc" '(flycheck-mode :which-key "flycheck")
        "td" '(dimmer-mode :which-key "dimmer")
        "tf" '(focus-mode :which-key "focus")
        "th" '(hl-line-mode :which-key "line highlight")
        "ti" '(aggressive-indent-mode :which-key "aggressive indent")
        "tl" '(lsp :which-key "LSP")
	    "tr" '(rainbow-mode :which-key "rainbow-mode")
        "tn" '(global-display-line-numbers-mode :which-key "line numbers")
        "tt" '(consult-theme :which-key "theme")
        "tw" '(writeroom-mode :which-key "writeroom")

        ;; QUIT
        "q"  '(:ignore t :which-key "quit")
        "qq" '(save-buffers-kill-emacs :which-key "save and quit")
        "qQ" '(kill-emacs :which-key "quit")

        ;; WINDOW
        "w"  '(:ignore t :which-key "window")
        "ww" '(kill-buffer-and-window :which-key "kill")

        ;; M-x
        "x" '(execute-extended-command :which-key "M-x")

        ;; PROFILER
        "y"  '(:ignore t :which-key "profiler")
        "ys" '(profiler-start :which-key "start")
        "yt" '(profiler-stop :which-key "stop")
        "yr" '(profiler-report :which-key "report")

        ;; NEXT & PREVIOUS
        "]"  '(:ignore t :which-key "next")
        "["  '(:ignore t :which-key "previous")
        "]e" '(flycheck-next-error :which-key "error")
        "[e" '(flycheck-previous-error :which-key "error")

        )
    )


;; Tabbing
(defun insert-tab-char ()
    "Insert a tab char (ASCII 9, \t)."
    (interactive) (insert "    "))
(setq
    indent-tabs-mode nil
    tab-width 4
    tab-stop-list (number-sequence 4 200 4)
    indent-line-function 'insert-tab
    backward-delete-char-untabify-method 'hungry
    lisp-indent-offset 4)

;; LANGUAGE CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C
(setq
    c-default-style "linux"
    c-basic-offset 4)
(c-set-offset 'comment-intro 0)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Proper undo tree
(use-package undo-fu
    :ensure t
    :commands (undo-fu-only-undo undo-fu-only-redo))
(when (package-installed-p 'undo-fu) ;; Undo-fu
    (setq-default evil-undo-system 'undo-fu)
    (evil-define-key 'normal 'global
        "u" 'undo-fu-only-undo
        "\C-r" 'undo-fu-only-redo))

;; Rainbow delimiters to make bracket spaghetti more bearable (could remove?)
(use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))

;; Save cursor position
(require 'saveplace)
(if (fboundp #'save-place-mode)
    (save-place-mode +1)
    (setq save-place-file "~/.cache/emacs_saveplace")
    (setq-default save-place t))

;; which-key
(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-popup-type 'minibuffer))

;; ;; Nicer modeline
;; (use-package nano-modeline
;;     :config
;;     (nano-modeline-mode)
;;     (setq nano-modeline-position 'bottom)
;;     (custom-set-faces
;;         '(mode-line ((t (:underline nil))))
;;         '(mode-line-inactive ((t (:underline nil))))))

;; Highlight colour codes with relevant colour
(use-package rainbow-mode :defer t)

;; Use system clipboard
(setq select-enable-clipboard t)

;; Theme

;; (use-package solarized-theme)
;; (setq solarized-use-more-italic t)
;; (load-theme 'solarized-dark t)

(use-package doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
        )
    (load-theme 'doom-solarized-dark t)
    (global-hl-line-mode 1) ; enable line highlighting

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    ;; (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    ;; (doom-themes-org-config)
    )

;; GUI CONFIGURATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fontfamily "Iosevka")
(defvar fontsans "FreeSans")
(defvar fontsize 12)
(defvar fontweight 'medium)

(defvar fontheight (* fontsize 10))
(defvar fontstring (format "%s-%s" fontfamily fontsize))
(defvar fontsizelarge (/ (* fontsize 6) 5))
(defvar fontheightlarge (* fontsizelarge 10))

;; Nicer-looking frame
(use-package frame
    :ensure nil
    :config
    (setq-default default-frame-alist
        (append (list
                    '(min-height . 1) '(min-width . 1)
                    '(internal-border-width . 24)
                    '(vertical-scroll-bars . nil)
                    '(left-fringe       . 1)
                    '(right-fringe      . 1)
                    '(tool-bar-lines    . 0)
                    '(menu-bar-lines    . 0)
                    )))
    (setq-default window-resize-pixelwise t)
    (setq-default frame-resize-pixelwise t)

    (set-face-attribute 'default nil
        :font fontfamily
        :weight fontweight
        :height fontheight)
    (set-face-attribute 'fixed-pitch nil
        :font fontfamily
        :weight fontweight
        :height fontheight)
    (set-face-attribute 'variable-pitch nil
        :font fontfamily
        :weight fontweight
        :height fontheightlarge)
    (set-face-attribute 'line-number nil
        :font fontfamily
        :weight fontweight
        :height fontheight)
    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-face-attribute 'default t :font fontstring)
    (set-frame-font fontstring)

    :custom
    (window-divider-default-right-width 2)
    (window-divider-default-bottom-width 2)
    (window-divider-default-places 'right-only)
    (window-divider-mode t))

(add-hook 'before-make-frame-hook 'window-divider-mode)

;; Disable visible scrollbar
(scroll-bar-mode -1)
;; Disable toolbar
(tool-bar-mode -1)
;; Some breathing room
(set-fringe-mode 10)
;; Don't use popup dialogue box when prompting
(setq use-dialog-box nil)

;; QOL
(setq
    scroll-margin 2
    delete-by-moving-to-trash t
    truncate-string-ellipsis "…"
    evil-want-fine-undo t
    browse-url-browser-function 'xwidget-webkit-browse-url)

;; Smoother scrolling
(setq
    redisplay-dont-pause t
    scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1)

;; One line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;; Don't accelerate scrolling
(setq mouse-wheel-progressive-speed nil)
;; Scroll window under mouse
(setq mouse-wheel-follow-mouse 't)

(global-subword-mode 1)
(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-large-scroll-height 20.0)
(setq pixel-scroll-precision-interpolation-factor 10)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Selectrum as a replacement for vertico, helm, and ido
(use-package selectrum
    :config
    (selectrum-mode +1)
    (marginalia-mode 1)
    (use-package selectrum-prescient
        :config
        (selectrum-prescient-mode 1)
        (prescient-persist-mode 1))
    )


(use-package consult
    :hook (completion-list-mode . consult-preview-at-point-mode)
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
    :config
    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")
    )

;; Info in the margins, for the minibuffer
(use-package marginalia
    :bind (("M-A" . marginalia-cycle)
              :map minibuffer-local-map
              ("M-A" . marginalia-cycle))
    :config
    (marginalia-mode))

;; These things are annoying and I do not need them
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq server-client-instructions nil)
(setq inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message t)
(setq server-client-instructions nil)

;; Move customisation variables to different file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Reset garbage collection to make gc pauses faster
(set 'gc-cons-threshold (* 50 1000 1000))

