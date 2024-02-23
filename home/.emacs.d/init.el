(load-theme 'modus-operandi t)

(setq user-emacs-directory "~/.emacs.d/")
(setq backup-directory-alist
    `(("." . ,(concat user-emacs-directory "backups"))))
(setq column-number-mode t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
(setq visible-bell nil)
(setq vc-follow-symlinks t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(savehist-mode 1)
(setq history-length 25)
(recentf-mode 1)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(add-to-list 'load-path "~/.emacs.d/packages/evil")
(require 'evil)
(setq evil-undo-system 'undo-fu)
(add-to-list 'load-path "~/.emacs.d/packages/undo-fu")
(require 'undo-fu)
(evil-mode)

(add-to-list 'load-path "~/.emacs.d/packages/reformatter")
(require 'reformatter)
(add-to-list 'load-path "~/.emacs.d/packages/zig-mode")
(autoload 'zig-mode "zig-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(require 'saveplace)
(if (fboundp #'save-place-mode)
    (save-place-mode +1)
    (setq save-place-file "~/.cache/emacs_saveplace")
    (setq-default save-place t))

(add-to-list 'load-path "~/.emacs.d/packages/which-key")
(require 'which-key)
(which-key-mode)
(setq which-key-popup-type 'minibuffer)

(setq select-enable-clipboard t)

; scroll behaviour
(setq
    redisplay-dont-pause t
    scroll-step 1
	scroll-conservatively 10000
	scroll-preserve-screen-position 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(global-subword-mode 1)
(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-large-scroll-height 20.0)
(setq pixel-scroll-precision-interpolation-factor 10)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq ring-bell-function 'ignore)
(setq server-client-instructions nil)
(setq inhibit-startup-message t)
(setq-default inhibit-startup-echo-area-message t)
(setq server-client-instructions nil)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

; GUI --------------------------------------------------------------------------
(defvar fontmono "CommitMono")
(defvar fontsans "Inter Display")
(defvar fontsize 14)
(defvar fontweight 'medium)

(defvar fontfamily fontsans)

(defvar fontheight (* fontsize 10))
(defvar fontstring (format "%s-%s" fontfamily fontsize))
(defvar fontsizelarge (/ (* fontsize 6) 5))
(defvar fontheightlarge (* fontsizelarge 10))

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

(window-divider-default-right-width 2)
(window-divider-default-bottom-width 2)
(window-divider-default-places 'right-only)
(window-divider-mode t)

(add-hook 'before-make-frame-hook 'window-divider-mode)
