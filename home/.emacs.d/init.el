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
(setq-default tab-width 4)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive) (insert "\t"))
(use-package evil
    :config
    (evil-mode 1)
    (evil-define-key 'insert 'global (kbd "C-<tab>") 'insert-tab-char)
    (use-package evil-commentary
        :config (evil-commentary-mode))
    (use-package evil-surround
        :config (global-evil-surround-mode 1)))

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
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
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
    (lsp-enable-which-key-integration t)
    (add-hook 'prog-mode-hook 'lsp))

;; autocomplete
(use-package company
    :init
    (global-set-key (kbd "<tab>")
        #'company-indent-or-complete-common)
    :config
    (global-company-mode))

;; flycheck
(use-package flycheck
    :config
    (global-flycheck-mode))

;; which key
(use-package which-key :config (which-key-mode))

;----------------------------------------------------------------------GUI-only
(defvar fontfamily "Iosevka")
(defvar fontsans "Fira Sans")
(defvar fontsize 12)
(defvar fontweight 'medium)

(defvar fontheight (* fontsize 10))
(defvar fontstring (format "%s-%s" fontfamily fontsize))
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
(add-to-list 'default-frame-alist `(font . ,fontstring))
(set-face-attribute 'default t :font fontstring)
(set-frame-font fontstring)

(set-frame-parameter nil 'internal-border-width 12)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(set-fringe-mode 10)    ; Give some breathing room

;; modeline coolness

(use-package all-the-icons
    :if (display-graphic-p))
(use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :config
    (setq doom-modeline-height 36)
    (setq doom-modeline-enable-word-count t))

;------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
