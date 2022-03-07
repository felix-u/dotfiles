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

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq inhibit-startup-message t)
(setq user-emacs-directory "~/.emacs.d/")

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Give some breathing room
(menu-bar-mode -1)      ; Disable the menubar

; Visible bell (DISABLE)
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

; Initialise package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

; Initialise use-package
(package-install 'use-package)
(require 'use-package)
(setq use-package-always-ensure t)

; Evil mode to make this bloody thing usable
(package-install 'evil)
(require 'evil)
(evil-mode 1)

; (unless (package-installed-p 'undo-tree)
;   (package-install 'undo-tree))
; (require 'undo-tree)
; (unless (package-installed-p 'undo-fu)
;   (package-install 'undo-fu))
; (require 'undo-fu)
; (unless (package-installed-p 'goto-chg)
;   (package-install 'goto-chg))
; (require 'goto-chg)

; ; gruvbox - TEMPORARY
; (unless (package-installed-p 'gruvbox-theme)
;   (package-install 'gruvbox-theme))
; (require 'gruvbox-theme)
; (load-theme 'gruvbox-dark-hard t)

; ; icons
; (unless (package-installed-p 'all-the-icons)
;   (package-install 'all-the-icons))
; (require 'all-the-icons)

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

(set-frame-parameter nil 'internal-border-width 12)

(add-to-list 'default-frame-alist '(font . "Iosevka-12" ))
(set-face-attribute 'default t :font "Iosevka-12" )
