;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

; (add-to-list 'load-path "~/.doom.d/manual")
; (require 'tokyonight)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Felix"
      user-mail-address "erm, no")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Iosevka Nerd Font" :height 120 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans" :weight 'medium :height 120))
(setq doom-font "Iosevka Medium 12"
      doom-variable-pitch-font "Fira Sans Medium 12")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-xresources)
; (setq doom-theme 'doom-moonlight)
; (add-to-list 'custom-theme-load-path "~/.doom.d/themes/")
; (load-theme 'doom-tokyo-night)

;; If you use `org" and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Have padding around all size
(push '(internal-border-width . 25) default-frame-alist)

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

; Navigate by visual line rather than actual line
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<up>") 'evil-previous-visual-line)

;; Window map keybinds
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;transparent adjustment
(set-frame-parameter (selected-frame)'alpha '(90 . 90))
(add-to-list 'default-frame-alist'(alpha . (90 . 90)))

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable nil; not anymore useful than flycheck
        lsp-ui-doc-enable nil
        lsp-enable-symbol-highlighting nil))


(setq undo-limit 80000000                          ;I mess up too much
      evil-want-fine-undo t                        ;By default while in insert all changes are one big blob. Be more granular
      scroll-margin 2                              ;having a little margin is nice
      auto-save-default t                          ;I dont like to lose work
      truncate-string-ellipsis "…"                 ;default ellipses suck
      browse-url-browser-function 'xwidget-webkit-browse-url) ;;use xwidgets as my browser

(setq-default delete-by-moving-to-trash t) ;delete to system trash instead
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t)) ;;stops flickering
(fringe-mode 0) ;;disable fringe
(global-subword-mode 1) ;;navigate through Camel Case words

; hide dividers
(custom-set-faces!
  `(vertical-border :background ,(doom-color 'bg) :foreground ,(doom-color 'bg)))

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places nil
        window-divider-default-bottom-width 0
        window-divider-default-right-width 0)
  (window-divider-mode -1))

;;disable cursorline
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)

;; periodic table
(use-package! eperiodic
  :commands eperiodic)

;;set treemacs to use the theme
(setq doom-themes-treemacs-theme "doom-colors")

;;use image previews
(setq org-startup-with-inline-images t)            ;inline images in org mode

(add-hook 'org-mode-hook 'turn-on-flyspell)

; essentially, turn hex code highlighting permanently on
(add-hook 'after-change-major-mode-hook 'rainbow-mode)

; ;; LaTeX -----------------------------------------------------------------------
; (setq lsp-tex-server 'digestif)
; (setq TeX-save-query nil
;       TeX-show-compilation t
;       TeX-command-extra-options "-shell-escape")
; (after! latex
;   (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

; (setq TeX-save-query nil
;       TeX-show-compilation t
;       TeX-command-extra-options "-shell-escape")
; (after! latex
;   (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

(setq +latex-viewers '(pdf-tools zathura evince okular skim sumatrapdf))
;; (latex-preview-pane-enable)
(setq LaTeX-biblatex-use-Biber t)
(setq TeX-command-BibTeX "Biber")
(setq bibtex-dialect 'biblatex)
(after! org
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t))))

(after! org
  (plist-put org-format-latex-options :background "Transparent"))

(add-hook 'LaTeX-mode-hook 'mixed-pitch-mode)
; (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

;; (after! org
;;   (add-hook 'org-mode-hook 'turn-on-org-cdlatex))

(defadvice! org-edit-latex-emv-after-insert ()
  :after #'org-cdlatex-environment-indent
  (org-edit-latex-environment))

(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-themed-minor-mode)
  :hook (pdf-tools-enabled . hide-mode-line-mode)
  :config
  (setq pdf-view-resize-factor 1.1)
  (setq-default pdf-view-display-size 'fit-page))

;; emacs org mode time format
(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %d.%m.%Y>" . "<%H:%M %a %d.%m.%Y>"))

;; disable kill prompt
(setq confirm-kill-emacs nil)

;; word count in modeline
(setq doom-modeline-enable-word-count t)

;; hide org markup indicators
(after! org (setq org-hide-emphasis-markers t))

;; Disable electric-mode, which is now respected by Org and which creates some confusing indentation sometimes.
(add-hook! org-mode (electric-indent-local-mode -1))

;; Enable variable and visual line mode in Org mode by default.
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

(add-hook 'org-mode-hook 'mixed-pitch-mode)

; org agenda files
(setq org-agenda-files '("~/uni/2021/autumn/agenda"))

; org-roam directory
(setq org-roam-directory "~/uni/2021/autumn/notes")
;; ensure that Org-roam is available on startup
(org-roam-db-autosync-mode)
; set completion-everywhere to true for faster linking
(setq org-roam-completion-everywhere t)

; org-roam-ui setup
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

; enable latex preview by default in org files
(setq org-startup-latex-with-latex-preview t)

; set width of org image previews
(setq org-image-actual-width 400)

; fix bogus gdscript errors, apparently.
; from https://github.com/godotengine/emacs-gdscript-mode
(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
;; Runs the function `lsp--gdscript-ignore-errors` around `lsp--get-message-type` to suppress unknown notification errors.
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)

(setq lsp-enable-on-type-formatting nil)
(setq c-basic-offset 4)
(setq tab-width 4)
;; collapse bullet points by default
;; (setq org-cycle-include-plain-lists 'integrate)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
