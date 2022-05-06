;; startup time trickery
(let ((file-name-handler-alist nil))
;; (set 'gc-cons-threshold 100000000)
(set 'gc-cons-threshold (* 100 1000 1000))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message t)
(setq server-client-instructions nil)
(setq user-emacs-directory "~/.emacs.d/")

(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable the menubar

;; better, usable terminal
(use-package vterm)
;; and package for better vterm binding
(use-package vterm-toggle)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; save all tempfiles in $TMPDIR/emacs$UID/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Visible bell (DISABLE)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq server-client-instructions nil)
;; relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t) ; show columns as well

;; don't ask about symlinks - just edit the file the link points to
(setq vc-follow-symlinks t)

;; automatic syntax pairs. might replace with a more intelligent plugin later
(electric-pair-mode 1)

;; save command history
(savehist-mode 1) (setq history-length 25)
;; save recent files
(recentf-mode 1)

;; enable ido to begin to use in keybinds
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(unless (featurep 'straight)
  ;; Bootstrap straight.el
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

; Initialise package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Initialise use-package
(straight-use-package 'use-package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

; Evil mode to make this bloody thing usable
;; tabbing
(setq indent-tabs-mode nil) 
(setq tab-width 4) 
(setq tab-stop-list (number-sequence 4 200 4))
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux")
(setq c-basic-offset 4)
(c-set-offset 'comment-intro 0)

(defun insert-tab-char ()
  "Insert a tab char (ASCII 9, \t)."
  (interactive) (insert "\t"))

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

    ;; delete, don't cut
    ;; (defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
    ;; (apply orig-fn beg end type ?_ args))
    ;; (advice-add 'evil-delete :around 'bb/evil-delete)

    ;; delete single character without yanking
    (define-key evil-normal-state-map "x" 'delete-forward-char)
    (define-key evil-normal-state-map "X" 'delete-backward-char)

    ;; window shortcuts (more below)
    (global-set-key (kbd "<C-return>") 'evil-window-new)
    (global-set-key (kbd "C-<next>") 'evil-window-split)
    (global-set-key (kbd "C-<end>") 'evil-window-vsplit)

    (use-package evil-commentary
        :config (evil-commentary-mode))
    (use-package evil-surround
      	:config (global-evil-surround-mode 1))
    (use-package avy ; like lightspeed.nvim
        :config
        (define-key evil-normal-state-map "S" 'avy-goto-char-2-above)
        (define-key evil-normal-state-map "s" 'avy-goto-char-2-below))
    (use-package frog-jump-buffer ; for switching buffers quickly
        :config
        (use-package all-the-icons-ivy)
        (setq frog-jump-buffer-use-all-the-icons-ivy t))
	(use-package general
		:config
		(general-evil-setup t)
		(general-create-definer rune/leader-keys
			:keymaps '(normal insert visual emacs)
			:prefix "SPC"
			:global-prefix "C-SPC"))

    ;; used in keymap below
    (defun kill-other-buffers ()
        "Kill all other buffers."
        (interactive)
        (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

    (rune/leader-keys

        "<SPC>" '(ido-find-file :which-key "find file")
        "." 	'(ido-dired :which-key "browse files (dired)")
        ","     '(frog-jump-buffer :which-key "switch buffer")

        "b"  '(:ignore t :which-key "buffer")
        "bb" '(frog-jump-buffer :which-key "switch buffer")
    	"bi" '(ibuffer :which-key "ibuffer")
        "bk" '(kill-buffer :which-key "kill buffer")
        "bo" '(kill-other-buffers :which-key "kill other buffers")
        "bs" '(save-buffer :which-key "save buffer")
        "b["  '(previous-buffer :which-key "prev buffer")
        "b]"  '(next-buffer :which-key "next buffer")

        "c"  '(:ignore t :which-key "code")
        "ce" '(eval-buffer :which-key "evaluate buffer")

        "f"  '(:ignore t :which-key "file")
        "f." '(ido-find-file :which-key "find file")
        "fr" '(recentf-open-files :which-key "recent files")

        "l"  '(:ignore t :which-key "lsp")
        "lr" '(iedit-mode :which-key "rename object (iedit)")
        "lf" '(flycheck-list-errors :which-key "flycheck errors")

        "o"  '(:ignore t :which-key "open")
        "ot" '(vterm-toggle :which-key "vterm")
	
        "t"  '(:ignore t :which-key "toggle")
        "tf" '(flycheck-mode :which-key "flycheck")
        "th" '(hl-line-mode :which-key "line highlight")
        "tl" '(lsp :which-key "LSP")
        "tn" '(global-display-line-numbers-mode :which-key "line numbers")
        "tt" '(load-theme :which-key "theme")

        "q"  '(:ignore t :which-key "quit")
        "qq" '(save-buffers-kill-emacs :which-key "save and quit")
        "qQ" '(kill-emacs :which-key "quit")

        "w"  '(:ignore t :which-key "window")
        "ww" '(kill-buffer-and-window :which-key "close")

        "x" '(execute-extended-command :which-key "M-x")

        )
    )

;; dwm-like tiling
(use-package edwina
    :config
    (setq display-buffer-base-action '(display-buffer-below-selected))
    (edwina-mode 1)
    (global-set-key (kbd "<C-left>") 'edwina-select-previous-window)
    (global-set-key (kbd "<C-up>") 'edwina-select-previous-window)
    (global-set-key (kbd "<C-right>") 'edwina-select-next-window)
    (global-set-key (kbd "<C-down>") 'edwina-select-next-window)

    (global-set-key (kbd "C-<S-left>") 'edwina-swap-previous-window)
    (global-set-key (kbd "C-<S-up>") 'edwina-swap-previous-window)
    (global-set-key (kbd "C-<S-right>") 'edwina-swap-next-window)
    (global-set-key (kbd "C-<S-down>") 'edwina-swap-next-window)

    (global-set-key (kbd "C-<M-left>") 'edwina-dec-mfact)
    (global-set-key (kbd "C-<M-right>") 'edwina-inc-mfact)
    (global-set-key (kbd "C-<M-up>") 'edwina-dec-nmaster)
    (global-set-key (kbd "C-<M-down>") 'edwina-inc-nmaster))

(straight-use-package 'evil-terminal-cursor-changer)
(unless (display-graphic-p)
	(require 'evil-terminal-cursor-changer)
	(evil-terminal-cursor-changer-activate)) ; or (etcc-on)

(use-package undo-tree
    :config
    (global-undo-tree-mode)
    (evil-set-undo-system 'undo-tree)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t ; if nil, italics is universally disabled
		doom-solarized-dark-padded-modeline t)
  (load-theme 'doom-solarized-dark t)
  (global-hl-line-mode 1) ; enable line highlighting

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; not working in terminal right now.
;; possible fix in docs https://github.com/belak/base16-emacs
(use-package base16-theme)
(setq custom-safe-themes t)

;; save cursor position
(require 'saveplace)
(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq save-place-file "~/.cache/emacs_saveplace")
  (setq-default save-place t))

;; treesitter - syntax highlighting and other cool shit
(use-package tree-sitter
    :config
    (use-package tree-sitter-langs)
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; vertical completion UI
(use-package vertico :config (vertico-mode))

;; marginalia in the minibuffer
(use-package marginalia
    :bind (("M-A" . marginalia-cycle)
        :map minibuffer-local-map
        ("M-A" . marginalia-cycle))
    :config
    (marginalia-mode))

;; lsp
(use-package lsp-mode
    :commands (lsp lsp-deferred)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
	(use-package iedit)
    (lsp-enable-which-key-integration t)
    (add-hook 'prog-mode-hook 'lsp))

;; autocomplete
(use-package company
    :init
    (global-set-key (kbd "<tab>")
        #'company-indent-or-complete-common)
    :config
	(setq company-minimum-prefix-length 1
		company-idle-delay 0.0) ;; default is 0.2
    (global-company-mode))

;; flycheck
(use-package flycheck
    :config
    (global-flycheck-mode)
    ;; only run flycheck on file save
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; which key
(use-package which-key
    :config
    (which-key-mode)
    (setq which-key-popup-type 'minibuffer))

;; modeline coolness
(use-package all-the-icons
    :if (display-graphic-p))
(use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :config
    (setq doom-modeline-height 36)
    (setq doom-modeline-enable-word-count t))

;; respond to prompts with y/n, not yes/no
(defalias 'yes-or-no #'y-or-n-p)

;; use system clipboard
(setq select-enable-clipboard t)

;; magit
(use-package magit)

;---------------------------------------------------------------language-configs
(use-package company-nixos-options
    :config
    (add-to-list 'company-backends 'company-nixos-options))
(use-package nix-mode
  :mode "\\.nix\\'")
;-------------------------------------------------------------------------------
    
;----------------------------------------------------------------------GUI-only
(defvar fontfamily "Iosevka")
(defvar fontsans "Fira Sans")
(defvar fontsize 12)
(defvar fontweight 'medium)

(defvar fontheight (* fontsize 10))
(defvar fontstring (format "%s-%s" fontfamily fontsize))

;; cleaner frame
(use-package frame
    :ensure nil
    :config
    (setq-default default-frame-alist
        (append (list
        '(internal-border-width . 16)
        '(left-fringe       . 0)
        '(right-fringe      . 0)
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
                        :font fontsans
                        :weight fontweight
                        :height fontheight)
    (set-face-attribute 'line-number nil
                        :font fontfamily
                        :weight fontweight
                        :height fontheight)

    ;; don't underline flycheck errors and warnings
    (set-face-attribute 'flycheck-error t
                        :underline nil)
    (set-face-attribute 'flycheck-warning t
                        :underline nil)

    (add-to-list 'default-frame-alist `(font . ,fontstring))
    (set-face-attribute 'default t :font fontstring)
    (set-frame-font fontstring)

    :custom
    (window-divider-default-right-width 2)
    (window-divider-default-bottom-width 2)
    (window-divider-default-places 'right-only)
    (window-divider-mode t))

(add-hook 'before-make-frame-hook 'window-divider-mode)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(set-fringe-mode 10)    ; Give some breathing room
(setq use-dialog-box nil) ; Don't use popup dialogue box when prompting

;------------------------------------------------------------------------------

;; move customisation variables to different file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; reset garbage collection to make gc pauses faster
(set 'gc-cons-threshold (* 2 1000 1000))

)
