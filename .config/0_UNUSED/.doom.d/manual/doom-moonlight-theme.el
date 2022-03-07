;;; doom-moonlight-theme.el --- inspired by VS code's Moonlight -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup doom-moonlight-theme nil
  "Options for the `doom-moonlight' theme."
  :group 'doom-themes)

(defcustom doom-moonlight-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-moonlight-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-moonlight
  "A dark theme inspired by VS code's Moonlight"

  ;; name        default   256       16
  ((bg         '("#1a1b26" "#1a1b26"  "black"))
   (bg-alt     '("#1a1b26" "#1a1b26" "black"))
   (base0      '("#212331" "#212331" "black"))
   (base1      '("#252737" "#252737" "brightblack"))
   (base2      '("#292b3d" "#292b3d" "brightblack"))
   (base3      '("#2d2f43" "#2d2f43" "brightblack"))
   ; (base4      '("#3d405a" "#3d405a" "brightblack"))
   (base4      '("#1a1b26" "#1a1b26" "brightblack"))
   (base5      '("#4e5173" "#4e5173" "brightblack"))
   (base6      '("#70738f" "#70738f" "brightblack"))
   (base7      '("#7c7fa0" "#7c7fa0" "brightblack"))
   (base8      '("#acb0d0" "#acb0d0" "white"))
   (indigo     '("#6578b3" "#6578b3" "brightblack"))
   (region     '("#3d405a" "#3d405a" "brightblack"))
   (fg         '("#dce0ef" "#dce0ef" "brightwhite"))
   (fg-alt     '("#acb0d0" "#acb0d0" "white"))

   (grey base5)

   (dark-red      '("#f44666" "#f44666" "red"))
   (red           '("#f7768e" "#f7768e" "red"))
   (light-red     '("#faa3b4" "#faa3b4" "brightred"))
   (orange        '("#ff9e64" "#ff9e64" "brightred"))
   (green         '("#6ace86" "#6ace86" "green"))
   (dark-teal     '("#3a7897" "#3a7897" "green"))
   (teal          '("#4a95ba" "#4a95ba" "brightgreen"))
   (light-teal    '("#6ba9c7" "#6ba9c7" "brightgreen"))
   (yellow        '("#e0af68" "#e0af68" "brightyellow"))
   (blue          '("#7aa2f7" "#7aa2f7" "brightblue"))
   (dark-blue     '("#4a80f4" "#4a80f4" "brightblue"))
   (light-blue    '("#a4bffa" "#a4bffa" "blue"))
   (light-magenta '("#b5a0d9" "#b5a0d9" "brightmagenta"))
   (magenta       '("#9a7ecc" "#9a7ecc" "brightmagenta"))
   (violet        '("#ce7bad" "#ce7bad" "magenta"))
   (light-pink    '("#daa2c7" "#daa2c7" "magenta"))
   (pink          '("#daa2c7" "#daa2c7" "magenta"))
   (cyan          '("#4a95ba" "#4a95ba" "brightcyan"))
   (dark-cyan     '("#6ba9c7" "#6ba9c7" "cyan"))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   base0)
   (line-highlight base4)
   (selection      region)
   (builtin        red)
   (comments       indigo)
   (doc-comments   (doom-lighten comments 0.25))
   (constants      orange)
   (functions      green)
   (keywords       red)
   (methods        red)
   (operators      dark-cyan)
   (type           blue)
   (strings        yellow)
   (variables      fg-alt)
   (numbers        magenta)
   (region         magenta)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    blue)
   (vc-added       teal)
   (vc-deleted     red)

   ;; custom categories
   (modeline-bg     (doom-darken base2 0.1))
   (modeline-bg-alt (doom-darken bg 0.1))
   (modeline-fg     base8)
   (modeline-fg-alt comments)

   (-modeline-pad
    (when doom-moonlight-padded-modeline
      (if (integerp doom-moonlight-padded-modeline) doom-moonlight-padded-modeline 4))))

  ;;;; Base theme face overrides
  ((font-lock-keyword-face :foreground keywords)
   (font-lock-comment-face :foreground comments)
   (font-lock-doc-face :foreground doc-comments)
   (hl-line :background line-highlight)
   (lazy-highlight :background base4 :foreground fg)
   ((line-number &override) :foreground base5 :background bg)
   ((line-number-current-line &override) :foreground fg :background line-highlight)
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-alt :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (tooltip :background base0 :foreground fg)

   ;;;; all-the-icons
   (all-the-icons-cyan       :foreground dark-cyan)
   (all-the-icons-cyan-alt   :foreground dark-cyan)
   (all-the-icons-dblue      :foreground (doom-darken blue 0.1))
   (all-the-icons-dgreen     :foreground dark-teal)
   (all-the-icons-dmaroon    :foreground magenta)
   (all-the-icons-dorange    :foreground orange)
   (all-the-icons-dpink      :foreground pink)
   (all-the-icons-dpurple    :foreground magenta)
   (all-the-icons-dred       :foreground dark-red)
   (all-the-icons-dsilver    :foreground grey)
   (all-the-icons-dyellow    :foreground orange)
   (all-the-icons-green      :foreground teal)
   (all-the-icons-lcyan      :foreground (doom-lighten dark-cyan 0.3))
   (all-the-icons-lgreen     :foreground green)
   (all-the-icons-lmaroon    :foreground light-magenta)
   (all-the-icons-lorange    :foreground orange)
   (all-the-icons-lpink      :foreground light-pink)
   (all-the-icons-lpurple    :foreground light-magenta)
   (all-the-icons-lred       :foreground light-red)
   (all-the-icons-lsilver    :foreground (doom-lighten grey 0.4))
   (all-the-icons-lyellow    :foreground (doom-lighten yellow 0.3))
   (all-the-icons-orange     :foreground orange)
   (all-the-icons-pink       :foreground pink)
   (all-the-icons-purple     :foreground magenta)
   (all-the-icons-purple-alt :foreground magenta)
   (all-the-icons-red-alt    :foreground red)
   (all-the-icons-silver     :foreground (doom-lighten grey 0.2))
   ;;;; all-the-icons-dired
   (all-the-icons-dired-dir-face :foreground indigo)
   ;;;; company
   (company-tooltip :inherit 'tooltip)
   (company-tooltip-common :foreground highlight)
   ;;;; company-box
   (company-box-annotation :foreground base7)
   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground green)
   (css-property             :foreground blue)
   (css-selector             :foreground red)
   ;;;; doom-emacs
   (doom-dashboard-menu-desc :foreground dark-cyan)
   (doom-dashboard-menu-tile :foreground dark-teal)
   ;;;; diredfl
   (diredfl-date-time    :foreground blue)
   (diredfl-file-name    :foreground base7)
   (diredfl-file-suffix  :foreground base6)
   (diredfl-symlink      :foreground dark-cyan)
   ;;;; dired+
   (diredp-number :foreground orange)
   ;;;; dired-k
   (dired-k-commited :foreground base4)
   (dired-k-modified :foreground vc-modified)
   (dired-k-ignored  :foreground cyan)
   (dired-k-added    :foreground vc-added)
   ;;;; doom-emacs
   (+workspace-tab-selected-face :background region :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-buffer-file       :foreground base7)
   (doom-modeline-icon-inactive     :foreground indigo)
   (doom-modeline-evil-normal-state :foreground dark-cyan)
   (doom-modeline-evil-insert-state :foreground blue)
   (doom-modeline-project-dir       :foreground light-teal)
   (doom-modeline-buffer-path       :foreground blue)
   (doom-modeline-buffer-modified :inherit 'bold :foreground yellow)
   (doom-modeline-buffer-major-mode :inherit 'doom-modeline-buffer-path)
   ;;;; ivy-posframe
   (ivy-posframe :background base0)
   (ivy-posframe-border :background base0)
   ;;;; js2-mode
   (js2-jsdoc-tag              :foreground magenta)
   (js2-object-property        :foreground dark-teal)
   (js2-object-property-access :foreground fg-alt)
   (js2-function-param         :foreground pink)
   (js2-jsdoc-type             :foreground base8)
   (js2-jsdoc-value            :foreground cyan)
   ;;;; linum
   ((linum &inherit line-number))
   ;;;; lsp-mode
   (lsp-face-highlight-read :background region)
   (lsp-face-highlight-textual :background region)
   (lsp-face-highlight-write :background region)
   (lsp-face-semhl-type-primative :foreground orange)
   (lsp-face-semhl-method :foreground magenta)
   ;;;; magit
   (magit-filename :foreground teal)
   ;;;; man <built-in>
   (Man-overstrike :inherit 'bold :foreground magenta)
   (Man-underline :inherit 'underline :foreground blue)
   ;;;; markdown-mode
   (markdown-header-face           :inherit 'bold :foreground yellow)
   (markdown-header-delimiter-face :inherit 'markdown-header-face)
   (markdown-metadata-key-face     :foreground magenta :inherit 'italic)
   (markdown-list-face             :foreground red)
   (markdown-url-face              :inherit 'underline :foreground orange)
   (markdown-gfm-checkbox-face     :foreground blue)
   (markdown-blockquote-face       :inherit 'italic :foreground fg)
   (mmm-default-submode-face       :background base1)
   ;;;; message <built-in>
   (message-header-name       :foreground green)
   (message-header-subject    :foreground highlight :weight 'bold)
   (message-header-to         :foreground highlight :weight 'bold)
   (message-header-cc         :inherit 'message-header-to :foreground (doom-darken highlight 0.15))
   (message-header-other      :foreground violet)
   (message-header-newsgroups :foreground yellow)
   (message-header-xheader    :foreground doc-comments)
   (message-separator         :foreground comments)
   (message-mml               :foreground comments :slant 'italic)
   (message-cited-text        :foreground magenta)
   ;;;; nav-flash
   (nav-flash-face :background region)
   ;;;; nix-mode
   (nix-attribute-face :foreground blue)
   (nix-builtin-face :foreground dark-teal)
   ;;;; org <built-in>
   ((outline-1 &override) :foreground light-blue)
   ((outline-2 &override) :foreground dark-cyan)
   ((outline-3 &override) :foreground light-red)
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground magenta)
   ((outline-6 &override) :foreground red)
   ((outline-7 &override) :foreground violet)
   ((org-block &override) :background base2)
   ((org-block-background &override) :background base2)
   ((org-block-begin-line &override) :background base2)
   ;;;; popup
   (popup-face :inherit 'tooltip)
   (popup-selection-face :inherit 'tooltip)
   ;;;; pos-tip
   (popup-tip-face :inherit 'tooltip)
   ;;;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground orange)
   (rainbow-delimiters-depth-3-face :foreground light-red)
   (rainbow-delimiters-depth-4-face :foreground cyan)
   (rainbow-delimiters-depth-5-face :foreground violet)
   (rainbow-delimiters-depth-6-face :foreground yellow)
   (rainbow-delimiters-depth-7-face :foreground blue)
   (rainbow-delimiters-depth-8-face :foreground teal)
   (rainbow-delimiters-depth-9-face :foreground dark-cyan)
   ;;;; rjsx-mode
   (rjsx-tag :foreground violet)
   (rjsx-attr :foreground yellow :slant 'italic :weight 'medium)
   ;;;; treemacs
   (treemacs-directory-face :foreground highlight)
   (treemacs-git-modified-face :foreground highlight)
   ;;;; which-key
   (which-key-command-description-face :foreground fg)
   (which-key-group-description-face :foreground magenta)
   (which-key-local-map-description-face :foreground cyan)))

;;; doom-moonlight-theme.el ends here
