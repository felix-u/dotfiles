(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message t)
(setq user-emacs-directory "~/.emacs.d/")

(tooltip-mode -1)       ; Disable tooltips
(menu-bar-mode -1)      ; Disable the menubar

;; Visible bell (DISABLE)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t) ; show columns as well

;; don't ask about symlinks - just edit the file the link points to
(setq vc-follow-symlinks t)

;; automatic syntax pairs. might replace with a more intelligent plugin later
(electric-pair-mode 1)

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

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

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
(setq use-package-always-ensure t)

; Evil mode to make this bloody thing usable
(straight-use-package 'evil)
(require 'evil)
(evil-mode 1)
(straight-use-package 'evil-terminal-cursor-changer)
(unless (display-graphic-p)
	(require 'evil-terminal-cursor-changer)
	(evil-terminal-cursor-changer-activate)) ; or (etcc-on)
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
;; tabbing
(setq-default tab-width 4)
(defun insert-tab-char ()
  "insert a tab char. (ASCII 9, \t)"
  (interactive) (insert "\t"))
(evil-define-key 'insert 'global (kbd "TAB") 'insert-tab-char)


(straight-use-package 'solarized-theme)
(require 'solarized-theme)
(load-theme 'solarized-dark t)
(setq solarized-use-more-italic t)
;; remove ugly modeline underline
(setq solarized-high-contrast-mode-line t)
(custom-set-faces
 '(mode-line ((t (:underline nil))))
 '(mode-line-inactive ((t (:underline nil)))))

;; not working in terminal right now.
;; possible fix in docs https://github.com/belak/base16-emacs
(straight-use-package 'base16-theme)
(setq custom-safe-themes t)

;; save cursor position
(require 'saveplace)
(if (fboundp #'save-place-mode)
  (save-place-mode +1)
  (setq save-place-file "~/.cache/emacs_saveplace")
  (setq-default save-place t))

;; treesitter - syntax highlighting and other cool shit
(straight-use-package 'tree-sitter)
(require 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;----------------------------------------------------------------------GUI-only
(set-face-attribute 'default nil
                    :font "Iosevka"
                    :weight 'medium
                    :height 120)
(set-face-attribute 'fixed-pitch nil
                    :font "Iosevka"
                    :weight 'medium
                    :height 120)
(set-face-attribute 'variable-pitch nil
                    :font "Fira Sans"
                    :weight 'medium
                    :height 120)
(set-face-attribute 'line-number nil
                    :font "Iosevka"
                    :weight 'medium
                    :height 120)
(add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
(set-face-attribute 'default t :font "Iosevka-12" )
(set-frame-font "Iosevka Nerd Font Medium 12")

(set-frame-parameter nil 'internal-border-width 12)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(set-fringe-mode 10)    ; Give some breathing room

;; modeline coolness
(straight-use-package 'nano-modeline)
(require 'nano-modeline)
(nano-modeline-mode 1)
;; (setq nano-modeline-position 'top)

;------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" default)))
