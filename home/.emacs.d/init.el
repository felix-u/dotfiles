;; startup time trickery
(let ((file-name-handler-alist nil))
(set 'gc-cons-threshold 100000000)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message t)
(setq server-client-instructions nil)
(setq user-emacs-directory "~/.emacs.d/")

(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable the menubar

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
(savehist-mode 1)

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
(setq-default indent-tabs-mode nil) (setq indent-tabs-mode nil)
(setq-default tab-width 4) (setq tab-width 4)
(setq indent-line-function 'insert-tab)
(defun insert-tab-char ()
  "Insert a tab char (ASCII 9, \t)."
  (interactive) (insert "\t"))
(use-package evil
    :config
    (evil-mode 1)
    (evil-define-key 'insert 'global (kbd "C-<tab>") 'insert-tab-char)
	;; delete, don't cut
		(defun bb/evil-delete (orig-fn beg end &optional type _ &rest args)
		(apply orig-fn beg end type ?_ args))
		(advice-add 'evil-delete :around 'bb/evil-delete)
    (use-package evil-commentary
        :config (evil-commentary-mode))
    (use-package evil-surround
      	:config (global-evil-surround-mode 1))
	(use-package general
		:config
		(general-evil-setup t)
		(general-create-definer rune/leader-keys
			:keymaps '(normal insert visual emacs)
			:prefix "SPC"
			:global-prefix "C-SPC"))

		(rune/leader-keys

		 	"<SPC>" '(find-file :which-key "find file")
			"." 	'(dired :which-key "browse files (dired)")

			"c"  '(:ignore t :which-key "code")
			"ce" '(eval-buffer :which-key "evaluate buffer")

			"f"  '(:ignore t :which-key "file")
			"f." '(find-file :which-key "find file")

			"l"  '(:ignore t :which-key "lsp")
			"lr" '(iedit-mode :which-key "rename object (iedit)")
            "lf" '(flycheck-list-errors :which-key "flycheck errors")

		  	"t"  '(:ignore t :which-key "toggle")
			"tt" '(load-theme :which-key "theme")
			"tf" '(flycheck-mode :which-key "flycheck")
			"tl" '(global-display-line-numbers-mode :which-key "line numbers")

			"q"  '(:ignore t :which-key "quit")
			"qq" '(save-buffers-kill-emacs :which-key "save and quit")
			"qQ" '(kill-emacs :which-key "quit")

		   )
	  )

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

;; lang-specific settings
(add-hook 'c-mode-hook
    (setq c-default-style "linux"
        c-basic-offset 4))

;; flycheck
(use-package flycheck
    :config
    (global-flycheck-mode)
    ;; only run flycheck on file save
    (setq flycheck-check-syntax-automatically '(save mode-enabled)))

;; which key
(use-package which-key :config (which-key-mode))

;; modeline coolness
(use-package all-the-icons
    :if (display-graphic-p))
(use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :config
    (setq doom-modeline-height 36)
    (setq doom-modeline-enable-word-count t))

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
    (window-divider-default-right-width 24)
    (window-divider-default-bottom-width 12)
    (window-divider-default-places 'right-only)
    (window-divider-mode t))

(add-hook 'before-make-frame-hook 'window-divider-mode)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(set-fringe-mode 10)    ; Give some breathing room

;------------------------------------------------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "66bdbe1c7016edfa0db7efd03bb09f9ded573ed392722fb099f6ac6c6aefce32" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "c4b0cd42365d27c859dc944197c9e5c36ff0725c66eb03fdf21dc8b004566388" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

)
