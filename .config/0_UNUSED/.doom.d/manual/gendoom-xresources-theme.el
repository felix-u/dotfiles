;; doom-xresources-theme.el --- inspired by Textmate's Monokai -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-xresources-theme nil
  "Options for doom-molokai."
  :group 'doom-themes)

(defcustom doom-xresources-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-xresources-theme
  :type 'boolean)

(defcustom doom-xresources-comment-bg doom-xresources-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-xresources-theme
  :type 'boolean)

(defcustom doom-xresources-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-xresources-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-xresources
  "A dark, vibrant theme inspired by Textmate's Monokai."

  ;; name        gui       256       16
  ((bg         '("$(xquery background)" nil       nil          ))
   (bg-alt     '("$(xquery background)" nil       nil          ))
   (base0      '("$(xquery background)" "$(xquery background)"   "black"      ))
   (base1      '("$(xquery background)" "$(xquery background)" "brightblack"))
   (base2      '("$(xquery background)" "$(xquery background)" "brightblack"))
   (base3      '("$(xquery color0)" "$(xquery color0)" "brightblack"))
   (base4      '("$(xquery color8)" "$(xquery color8)" "brightblack"))
   (base5      '("$(xquery color8)" "$(xquery color8)" "brightblack"))
   (base6      '("$(xquery color7)" "$(xquery color7)" "brightblack"))
   (base7      '("$(xquery color7)" "$(xquery color7)" "brightblack"))
   (base8      '("$(xquery color15)" "$(xquery color15)" "brightwhite"))
   (fg         '("$(xquery foreground)" "$(xquery foreground)" "brightwhite"))
   (fg-alt     '("$(xquery color7)" "$(xquery color7)" "white"))

   (color0      '("$(xquery color0)" "$(xquery color0)" "color0"))
   (color8      '("$(xquery color8)" "$(xquery color8)" "color8"))
   (grey       '("$(xquery color7)" "$(xquery color7)" "brightblack"))
   (red        '("$(xquery color1)" "$(xquery color1)" "red"))
   (orange     '("$(xquery orange)" "$(xquery orange)" "brightred"))
   (green      '("$(xquery color2)" "$(xquery color2)" "green"))
   (teal       green)
   (yellow     '("$(xquery color3)" "$(xquery color3)" "yellow"))
   (blue       '("$(xquery color12)" "$(xquery color12)" "brightblue"))
   (dark-blue  '("$(xquery color4)" "$(xquery color4)" "blue"))
   (magenta    '("$(xquery color5)" "$(xquery color5)" "magenta"))
   (violet     '("$(xquery color13)" "$(xquery color13)" "brightmagenta"))
   (cyan       '("$(xquery color14)" "$(xquery color14)" "brightcyan"))
   (dark-cyan  '("$(xquery color6)" "$(xquery color6)" "cyan"))

   ;; face categories
   (highlight      dark-blue)
   (vertical-bar   (doom-lighten bg 0.1))
   (selection      base5)
   (builtin        cyan)
   (comments       (if doom-xresources-brighter-comments violet base5))
   (doc-comments   (if doom-xresources-brighter-comments (doom-lighten violet 0.1) (doom-lighten base5 0.25)))
   (constants      red)
   (functions      dark-blue)
   (keywords       magenta)
   (methods        green)
   (operators      dark-blue)
   (type           yellow)
   (strings        green)
   (variables      red)
   (numbers        orange)
   (region         base4)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    cyan)
   (vc-added       (doom-darken green 0.15))
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-pad
    (when doom-xresources-padded-modeline
      (if (integerp doom-xresources-padded-modeline) doom-xresources-padded-modeline 4)))

   (modeline-fg nil)
   (modeline-fg-alt base4)

   (modeline-bg base1)
   (modeline-bg-inactive (doom-darken base2 0.2))

   (org-quote `(,(doom-lighten (car bg) 0.05) "#1f1f1f")))


  ;;;; Base theme face overrides
  ((cursor :background red)
   ((font-lock-comment-face &override) :slant 'italic)
   ((font-lock-type-face &override) :slant 'italic)
   (highlight-numbers-number :foreground numbers :weight 'normal)
   (lazy-highlight :background dark-cyan :foreground base0 :distant-foreground base0 :bold bold)
   ((line-number &override) :foreground base5 :distant-foreground nil)
   ((line-number-current-line &override) :foreground base8 :distant-foreground nil)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color modeline-bg-inactive)))

   ;;;; centaur-tabs
   (centaur-tabs-selected-modified :inherit 'centaur-tabs-selected
                                   :background bg
                                   :foreground yellow)
   (centaur-tabs-unselected-modified :inherit 'centaur-tabs-unselected
                                     :background bg-alt
                                     :foreground yellow)
   (centaur-tabs-active-bar-face :background yellow)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected :foreground fg)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected :foreground fg)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground keywords)
   ;;;; doom-modeline
   (doom-modeline-bar :background yellow)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'bold :foreground green)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   (doom-modeline-buffer-modified :inherit 'bold :foreground orange)


   (isearch :foreground base0 :background green)
   ;;;; ediff <built-in>
   (ediff-fine-diff-A :background (doom-blend magenta bg 0.3) :weight 'bold)
   ;;;; evil
   (evil-search-highlight-persist-highlight-face :background violet)
   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background green)
   (evil-snipe-matches-face     :foreground green :underline t)
   ; (highlight-numbers-number :foreground orange :weight 'bold)
   ;;;; flycheck
   (flycheck-error   :underline `(:style wave :color ,red)    :background base3)
   (flycheck-warning :underline `(:style wave :color ,yellow) :background base3)
   (flycheck-info    :underline `(:style wave :color ,green)  :background base3)
   ;;;; helm
   (helm-swoop-target-line-face :foreground magenta :inverse-video t)
   ;;;; ivy
   (ivy-current-match :background base3)
   (ivy-minibuffer-match-face-1 :background base1 :foreground base4)
   ;;;; LaTeX
   (font-latex-script-char-face :foreground red)
   (font-latex-verbatim-face :foreground orange)
   (font-latex-warning-face :foreground red)
   (font-latex-string-face :foreground yellow)
   (font-latex-sedate-face :foreground dark-cyan)
   (font-latex-math-face :foreground orange)
   (font-latex-italic-face :foreground green)
   (font-latex-bold-face :foreground green)
   (font-latex-sectioning-0-face :foreground yellow)
   (font-latex-sectioning-1-face :foreground yellow)
   (font-latex-sectioning-2-face :foreground yellow)
   (font-latex-sectioning-3-face :foreground yellow)
   (font-latex-sectioning-4-face :foreground yellow)
   (font-latex-sectioning-5-face :foreground yellow)
   ;;;; whitespace
   (whitespace-tab :background bg)
   ;;;; markdown-mode
   (markdown-blockquote-face :inherit 'italic :foreground dark-blue)
   (markdown-list-face :foreground magenta)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground blue)
   ((markdown-code-face &override) :background (doom-lighten base2 0.045))
   ;;;; neotree
   (neo-dir-link-face   :foreground cyan)
   (neo-expand-btn-face :foreground magenta)
   ;;;; outline <built-in>
   ((outline-1 &override) :foreground magenta)
   ((outline-2 &override) :foreground orange)
   ;;;; org <built-in>
   (org-ellipsis :foreground orange)
   (org-tag :foreground yellow :bold nil)
   ((org-quote &override) :inherit 'italic :foreground base7 :background org-quote)
   (org-todo :foreground yellow :bold 'inherit)
   (org-list-dt :foreground yellow)
   (org-level-1 :foreground dark-blue)
   (org-level-2 :foreground red)
   (org-level-3 :foreground magenta)
   (org-checkbox-statistics-todo :foreground red :background color0)
   (org-checkbox-statistics-done :foreground green :background color0)
   (org-superstar-header-bullet :foreground dark-blue)
   (org-done :foreground green :background color0)
   (org-headline-done :foreground color8)
   (org-special-keyword :foreground red)
   (org-date :foreground magenta)
   (org-document-info-keyword :foreground green)
   (org-document-title :foreground red)
   (org-todo :foreground red :background color0)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground green)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground orange)
   (rainbow-delimiters-depth-7-face :foreground green))


  ;;;; Base theme variable overrides
  ;; ()
  )

;;; doom-xresources-theme.el ends here
