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
    ;; (global-display-line-numbers-mode 1)
    ;; (setq display-line-numbers-type 'relative)
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

    ;; Initialise package sources
    (require 'package)
    (setq package-archives '(   ("melpa" . "https://melpa.org/packages/")
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

    ;; Evil mode to make this bloody thing usable
    ;; tabbing
    (setq indent-tabs-mode nil
        tab-width 4
        tab-stop-list (number-sequence 4 200 4)
        indent-line-function 'insert-tab
        backward-delete-char-untabify-method 'hungry

        c-default-style "linux"
        c-basic-offset 4

        lisp-indent-offset 4)


    ;; aggressive indent
    (use-package aggressive-indent
        :config
        (add-hook 'prog-mode-hook #'aggressive-indent-mode))

    ;; selectrum as a replacement for vertico, helm, and ido
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

    ;; marginalia in the minibuffer
    (use-package marginalia
        :bind (("M-A" . marginalia-cycle)
                  :map minibuffer-local-map
                  ("M-A" . marginalia-cycle))
        :config
        (marginalia-mode))

    ;; fast actions with embark
    (use-package embark
	    :bind
	    (("C-." . embark-act)
	        ("C-," . embark-dwim))
	    :init
	    ;; Optionally replace the key help with a completing-read interface
	    (setq prefix-help-command #'embark-prefix-help-command)
	    ;; check repo for some extra integration with consult
        )

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
            "tl" '(lsp :which-key "LSP")
            "tn" '(global-display-line-numbers-mode :which-key "line numbers")
            "tt" '(consult-theme :which-key "theme")
            "tw" '(writeroom-mode :which-key "writeroom")

            ;; QUIT
            "q"  '(:ignore t :which-key "quit")
            "qq" '(save-buffers-kill-emacs :which-key "save and quit")
            "qQ" '(kill-emacs :which-key "quit")

            ;; WINDOW
            "w"  '(:ignore t :which-key "window")
            "ww" '(kill-buffer-and-window :which-key "close")

            ;; M-x
            "x" '(execute-extended-command :which-key "M-x")

            ;; NEXT & PREVIOUS
            "]"  '(:ignore t :which-key "next")
            "["  '(:ignore t :which-key "previous")
            "]e" '(flycheck-next-error :which-key "error")
            "[e" '(flycheck-previous-error :which-key "error")

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

    ;; (use-package undo-tree
    ;;     :config
    ;;     (global-undo-tree-mode)
    ;;     (evil-set-undo-system 'undo-tree)
    ;;     (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
    (use-package undo-fu
        :ensure t
        :commands (undo-fu-only-undo undo-fu-only-redo))
    (when (package-installed-p 'undo-fu) ;; Undo-fu
        (setq-default evil-undo-system 'undo-fu)
        (evil-define-key 'normal 'global
            "u" 'undo-fu-only-undo
            "\C-r" 'undo-fu-only-redo))

    (use-package doom-themes
        :config
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
            doom-themes-enable-italic t ; if nil, italics is universally disabled
            )
        (load-theme 'doom-solarized-dark t)
        ;; (global-hl-line-mode 1) ; enable line highlighting

        ;; Enable custom neotree theme (all-the-icons must be installed!)
        ;; (doom-themes-neotree-config)
        ;; or for treemacs users
        ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
        ;; (doom-themes-treemacs-config)
        ;; Corrects (and improves) org-mode's native fontification.
        ;; (doom-themes-org-config)
        )

    ;; rainbow delimiters to make bracket spaghetti more bearable
    (use-package rainbow-delimiters
        :hook
        (prog-mode . rainbow-delimiters-mode))
    
    ;; projectile
    (use-package projectile
        :config
        (projectile-mode 1)
        (setq projectile-enable-caching t))

    ;; better, usable terminal
    (use-package vterm)
    ;; and package for better vterm binding
    (use-package vterm-toggle)

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

    ;; lsp
    (use-package lsp-mode
        :commands (lsp lsp-deferred)
        :init
        (setq lsp-keymap-prefix "C-c l")
        :config
        (use-package iedit)
        (lsp-enable-which-key-integration t)
        (add-hook 'prog-mode-hook 'lsp)
        (use-package lsp-ui
            :hook (lsp-mode . lsp-ui-mode)
            :config
            (setq lsp-lens-enable t
                lsp-ui-doc-enable t
                lsp-ui-sideline-enable nil
                lsp-enable-symbol-highlighting t
                lsp-enable-semantic-tokens-enable t
                lsp-headerline-breadcrumb-enable nil)))

    ;; autocomplete
    (use-package company
        :init
        (global-set-key (kbd "\t")
            #'company-indent-or-complete-common)
        :config
        (setq company-minimum-prefix-length 1
            company-idle-delay 0.0) ;; default is 0.2
        (global-company-mode t)
        (use-package company-prescient
            :init
            (company-prescient-mode +1))
        )

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

    ;; (use-package doom-modeline
    ;;     :hook (after-init . doom-modeline-mode)
    ;;     :config
    ;;     (setq doom-modeline-height 36)
    ;;     (doom-solarized-dark-padded-modeline t)
    ;;     (setq doom-modeline-enable-word-count t))

    (use-package nano-modeline
        :config
        (nano-modeline-mode)
        (custom-set-faces
            '(mode-line ((t (:underline nil))))
            '(mode-line-inactive ((t (:underline nil))))))

    ;; respond to prompts with y/n, not yes/no
    (defalias 'yes-or-no #'y-or-n-p)

    ;; use system clipboard
    (setq select-enable-clipboard t)

    ;; magit
    (use-package magit)

    ;; "auto-activating-snippets" - not yet set up!
    ;; (use-package aas)
    ;; (use-package laas) ; aas snippets for LaTeX

    ;;---------------------------------------------------------------writing-focused
    (use-package writeroom-mode
        :defer t
        :config
        (use-package visual-fill-column
            :defer t
            :config
  	        (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  	        (setq-default visual-fill-column-center-text t)))

    ;; for reading epub - https://depp.brause.cc/nov.el/
    ;; (use-package nov)
    ;;------------------------------------------------------------------------------

    ;; dim surrounding text - larstvei/Focus
    (use-package focus
        :defer t)
    ;; in a similar vein, dim unfocused buffers
    (use-package dimmer
        :defer t
        :config
        (dimmer-configure-which-key)
        (dimmer-configure-company-box))

    ;; extra colours for info mode
    (use-package info-colors
        :defer t)
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)

    ;; dashboard
    (use-package dashboard
        :config
        (dashboard-setup-startup-hook)
        (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
        (setq dashboard-banner-logo-title "GNU Emacs version 29")
        (setq dashboard-center-content t)
        (setq dashboard-show-shortcuts nil)
        (setq dashboard-items '())
        (defun dashboard-insert-custom (list-size)
            (insert "\n\nGNU Emacs"))
        (add-to-list 'dashboard-item-generators  '(custom . dashboard-insert-custom))
        (add-to-list 'dashboard-items '(custom) t)
        (setq dashboard-set-footer nil)
        (setq dashboard-startup-banner nil)
        )

    ;;-----------------------------------------------------------language-configs
    ;; nix
    (use-package company-nixos-options
        :defer t
        :config
        (add-to-list 'company-backends 'company-nixos-options))
    (use-package nix-mode
        :defer t
        :mode "\\.nix\\'")

    ;; LaTeX
    (use-package tex ; support for latex
        :defer t
        :ensure auctex
        :config
        (setq TeX-auto-save t)
        (setq TeX-parse-self t)
        (setq-default TeX-master nil)
        )
    ;; (use-package preview-latex :defer t)
    (use-package latex-preview-pane
        :defer t
        :config
        (latex-preview-pane-enable))
    (use-package evil-tex ; successor to evil-latex-textobjects, inspired by vimtex
        :config
        (add-hook 'LaTeX-mode-hook #'evil-tex-mode))
    ;; to use pdfview with auctex
    (use-package pdf-tools
        :config
        (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
            TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
            TeX-source-correlate-start-server t) ;; not sure if last line is neccessary
        ;; to have the buffer refresh after compilation
        (add-hook 'TeX-after-compilation-finished-functions
	        #'TeX-revert-document-buffer)
        )
    ;;--------------------------------------------------------------------------


    ;;------------------------------------------------------------------GUI-only
    (defvar fontfamily "Iosevka")
    (defvar fontsans "Fira Sans")
    (defvar fontserif "IBM Plex Serif")
    (defvar fontsize 12)
    (defvar fontweight 'medium)

    (defvar fontheight (* fontsize 10))
    (defvar fontstring (format "%s-%s" fontfamily fontsize))
    (defvar fontsizelarge (/ (* fontsize 6) 5))
    (defvar fontheightlarge (* fontsizelarge 10))

    ;; cleaner frame
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

        ;; don't underline flycheck errors and warnings
        (set-face-attribute 'flycheck-error nil
            :underline nil)
        (set-face-attribute 'flycheck-warning nil
            :underline nil)
        (set-face-attribute 'flymake-note nil
            :underline nil)
        (set-face-attribute 'flycheck-info nil
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

    ;; some more better settings
    (setq scroll-margin 2
        delete-by-moving-to-trash t
        truncate-string-ellipsis "…"
        evil-want-fine-undo t
        browse-url-browser-function 'xwidget-webkit-browse-url)

    ;; smoother scrolling
    (setq scroll-step 1
	    scroll-conservatively 10000
	    scroll-preserve-screen-position 1)

    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

    (global-subword-mode 1)
    (pixel-scroll-precision-mode t)
    (setq pixel-scroll-precision-large-scroll-height 20.0)
    (setq pixel-scroll-precision-interpolation-factor 10)
    (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


    ;;--------------------------------------------------------------------------

    ;;-----------------------------------------------------------------------ORG
    (use-package org
        :config
        (setq org-ellipsis " …")
	    (setq org-hide-emphasis-markers t)
	    (add-hook 'org-mode-hook 'variable-pitch-mode))

    (use-package org-bullets
	    :after org
	    :hook (org-mode . org-bullets-mode))

    ;; replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
	    '(("^ *\\([-]\\) "
	          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; proportional fonts for org mode
    (let* (   (variable-tuple     '(:font "IBM Plex Serif"))
              (base-font-color     (face-foreground 'default nil 'default))
              (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

        ;; org level font settings
        (custom-theme-set-faces
            'user
            `(org-level-8 ((t (,@headline ,@variable-tuple))))
            `(org-level-7 ((t (,@headline ,@variable-tuple))))
            `(org-level-6 ((t (,@headline ,@variable-tuple))))
            `(org-level-5 ((t (,@headline ,@variable-tuple))))
            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
            `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

    ;; ;; variable pitch definitions
    ;; (custom-theme-set-faces
    ;;     'user
    ;;     '(variable-pitch ((t (:family fontserif :height 160 :weight medium))))
    ;;     '(fixed-pitch ((t (:family fontfamily :height 120)))))

    ;; ;; headings go from bigger to smaller
    (dolist (face '((org-level-1 . 1.15)
		               (org-level-2 . 1.12)
		               (org-level-3 . 1.06)
		               (org-level-4 . 1.03)
		               (org-level-5 . 1.0)
		               (org-level-6 . 1.0)
		               (org-level-7 . 1.0)
		               (org-level-8 . 1.0)))
	    (set-face-attribute (car face) nil :font fontserif :weight fontweight :height (cdr face)))

    ;;--------------------------------------------------------------------------


    ;; move customisation variables to different file and load it
    (setq custom-file (locate-user-emacs-file "custom-vars.el"))
    (load custom-file 'noerror 'nomessage)

    ;; reset garbage collection to make gc pauses faster
    (set 'gc-cons-threshold (* 2 1000 1000))

    )
