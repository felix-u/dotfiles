import subprocess

# try to calm down LSP
config = config
c = c

config.set("colors.webpage.darkmode.enabled", False)
config.set("fonts.default_size", "12pt")
config.load_autoconfig(False)

config.set("zoom.default", "100%")
config.set("qt.highdpi", True)

term = '$TERMINAL'
colours = {
        'background': '$(wq background)',
        'foreground': '$(wq foreground)',
        'color0': '$(wq color0)',
        'color1': '$(wq color1)',
        'color2': '$(wq color2)',
        'color3': '$(wq color3)',
        'color4': '$(wq color4)',
        'color5': '$(wq color5)',
        'color6': '$(wq color6)',
        'color7': '$(wq color7)',
        'color8': '$(wq color8)',
        'color9': '$(wq color9)',
        'color10': '$(wq color10)',
        'color11': '$(wq color11)',
        'color12': '$(wq color12)',
        'color13': '$(wq color13)',
        'color14': '$(wq color14)',
        'color15': '$(wq color15)',
        'fontmono': '$(wq fontmono)'
        }

config.set("fonts.default_family", colours['fontmono'])


#                      ____
#     ____  ________  / __/____
#    / __ \/ ___/ _ \/ /_/ ___/
#   / /_/ / /  /  __/ __(__  )
#  / .___/_/   \___/_/ /____/
# /_/
#
#
#
#
# refer to
# https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/settings.asciidoc

# completion menu -------------------------------------------------------------
c.colors.completion.category.bg = colours['color0']
c.colors.completion.category.fg = colours['foreground']
#
c.colors.completion.category.border.bottom = colours['background']
c.colors.completion.category.border.top = colours['background']
#
c.colors.completion.even.bg = colours['background']
c.colors.completion.odd.bg = colours['background']
c.colors.completion.fg = colours['color15']
#
c.colors.completion.item.selected.bg = colours['color8']
c.colors.completion.item.selected.border.bottom = colours['color8']
c.colors.completion.item.selected.border.top = colours['color8']
c.colors.completion.item.selected.fg = colours['foreground']
#
c.colors.completion.match.fg = colours['color2']
#
c.colors.completion.scrollbar.bg = colours['color0']
c.colors.completion.scrollbar.fg = colours['color7']
# -----------------------------------------------------------------------------

# context menu (right click) --------------------------------------------------
c.colors.contextmenu.disabled.bg = colours['color0']
c.colors.contextmenu.disabled.fg = colours['color7']
c.colors.contextmenu.menu.bg = colours['color0']
c.colors.contextmenu.menu.fg = colours['color15']
c.colors.contextmenu.selected.bg = colours['color8']
c.colors.contextmenu.selected.fg = colours['foreground']
# -----------------------------------------------------------------------------

# download bar ----------------------------------------------------------------
c.colors.downloads.bar.bg = colours['color0']
#
c.colors.downloads.error.bg = colours['color1']
c.colors.downloads.error.fg = colours['background']
#
c.colors.downloads.start.bg = colours['background']
c.colors.downloads.start.fg = colours['foreground']
c.colors.downloads.stop.bg = colours['background']
c.colors.downloads.stop.fg = colours['foreground']
#
c.colors.downloads.system.bg = 'none'
c.colors.downloads.system.fg = 'none'
# -----------------------------------------------------------------------------

# hints (labels on links) -----------------------------------------------------
c.colors.hints.bg = colours['color3']
c.colors.hints.fg = colours['background']
c.colors.hints.match.fg = colours['background']
c.colors.keyhint.bg = colours['color0']
c.colors.keyhint.fg = colours['foreground']
c.colors.keyhint.suffix.fg = colours['color5']
#
c.hints.border = '0px solid #000000'
c.hints.mode = 'number'
# -----------------------------------------------------------------------------

# info and error messages -----------------------------------------------------
c.colors.messages.error.bg = colours['color1']
c.colors.messages.error.border = colours['color1']
c.colors.messages.error.fg = colours['background']
c.colors.messages.info.bg = colours['color0']
c.colors.messages.info.border = colours['color0']
c.colors.messages.info.fg = colours['foreground']
c.colors.messages.warning.bg = colours['color3']
c.colors.messages.warning.border = colours['color3']
c.colors.messages.warning.fg = colours['background']
# -----------------------------------------------------------------------------

# prompts ---------------------------------------------------------------------
c.colors.prompts.bg = colours['color0']
c.colors.prompts.border = colours['color0']
c.colors.prompts.fg = colours['color15']
c.colors.prompts.selected.bg = colours['color8']
c.colors.prompts.selected.fg = colours['foreground']
# -----------------------------------------------------------------------------

# statusbar -------------------------------------------------------------------
c.colors.statusbar.caret.bg = colours['color6']
c.colors.statusbar.caret.fg = colours['background']
c.colors.statusbar.caret.selection.bg = colours['color4']
c.colors.statusbar.caret.selection.fg = colours['background']
#
c.colors.statusbar.command.bg = colours['color0']
c.colors.statusbar.command.fg = colours['foreground']
c.colors.statusbar.command.private.bg = colours['color5']
c.colors.statusbar.command.private.fg = colours['background']
#
c.colors.statusbar.insert.bg = colours['color0']
c.colors.statusbar.insert.fg = colours['color2']
#
c.colors.statusbar.normal.bg = colours['color0']
c.colors.statusbar.normal.fg = colours['foreground']
#
c.colors.statusbar.passthrough.bg = colours['color4']
c.colors.statusbar.passthrough.fg = colours['background']
#
c.colors.statusbar.private.bg = colours['color5']
c.colors.statusbar.private.fg = colours['background']
#
c.colors.statusbar.progress.bg = colours['color8']
#
c.colors.statusbar.url.error.fg = colours['color1']
c.colors.statusbar.url.fg = colours['foreground']
c.colors.statusbar.url.hover.fg = colours['color6']
c.colors.statusbar.url.success.http.fg = colours['color3']
c.colors.statusbar.url.success.https.fg = colours['color2']
c.colors.statusbar.url.warn.fg = colours['color1']
#
c.statusbar.padding = {"bottom": 6, "left": 4, "right": 4, "top": 6}
c.statusbar.show = "in-mode"
# -----------------------------------------------------------------------------

# tabs ------------------------------------------------------------------------
c.colors.tabs.bar.bg = colours['background']
c.colors.tabs.even.bg = colours['background']
c.colors.tabs.even.fg = colours['color7']
c.colors.tabs.odd.bg = colours['background']
c.colors.tabs.odd.fg = colours['color7']
#
c.colors.tabs.indicator.error = colours['color1']
c.colors.tabs.indicator.start = colours['color4']
c.colors.tabs.indicator.stop = colours['color4']
c.colors.tabs.indicator.system = 'none'
#
c.colors.tabs.pinned.even.bg = colours['background']
c.colors.tabs.pinned.even.fg = colours['foreground']
c.colors.tabs.pinned.odd.bg = colours['background']
c.colors.tabs.pinned.odd.fg = colours['foreground']
c.colors.tabs.pinned.selected.even.bg = colours['color0']
c.colors.tabs.pinned.selected.even.fg = colours['foreground']
c.colors.tabs.pinned.selected.odd.bg = colours['color0']
c.colors.tabs.pinned.selected.odd.fg = colours['foreground']
#
c.colors.tabs.selected.even.bg = colours['color0']
c.colors.tabs.selected.odd.bg = colours['color0']
c.colors.tabs.selected.even.fg = colours['foreground']
c.colors.tabs.selected.odd.fg = colours['foreground']
#
c.tabs.show = "multiple"
c.tabs.indicator.padding = {"bottom": 4, "left": 1, "right": 8, "top": 4}
c.tabs.last_close = 'close'
c.tabs.padding = {"bottom": 6, "left": 9, "right": 9, "top": 6}
c.tabs.select_on_remove = 'prev'
# -----------------------------------------------------------------------------

# webpage ---------------------------------------------------------------------
c.colors.webpage.bg = colours['background']
c.colors.webpage.preferred_color_scheme = 'dark'
# -----------------------------------------------------------------------------

# content settings ------------------------------------------------------------
c.content.blocking.enabled = True
c.content.blocking.method = 'both'
c.content.cookies.accept = "no-3rdparty"
# -----------------------------------------------------------------------------

# downloads -------------------------------------------------------------------
c.downloads.location.remember = False
c.downloads.position = 'bottom'
# -----------------------------------------------------------------------------

# fileselect ------------------------------------------------------------------
c.fileselect.folder.command = [term, "-e", "nnn",  "-p", "{}"]
c.fileselect.handler = 'external'
c.fileselect.multiple_files.command = [term, "-e", "nnn",  "-p", "{}"]
c.fileselect.single_file.command = [term, "-e", "nnn",  "-p", "{}"]
# -----------------------------------------------------------------------------

# input behaviour -------------------------------------------------------------
c.input.insert_mode.auto_load = True
# -----------------------------------------------------------------------------

# new instance behaviour ------------------------------------------------------
c.new_instance_open_target = 'window'
# -----------------------------------------------------------------------------

# url -------------------------------------------------------------------------
# google for now, because brave doesn't work with stripped down dark reader
c.url.default_page = "https://google.com"
c.url.searchengines = {"DEFAULT": "https://google.com/search?q={}"}
c.url.start_pages = "https://google.com"
# -----------------------------------------------------------------------------

# editor ----------------------------------------------------------------------
c.editor.command = [term, "-e", "nvim", "{file}", "-c", "{line}"]
# -----------------------------------------------------------------------------

#
# bindings
#
#

# open videos with mpv
config.bind(",m", 'hint links spawn mpv {hint-url}')

# toggle custom stylesheet
config.bind(",d", "config-cycle content.user_stylesheets '~/dotfiles/misc/css/everything.css' '' ")

# clear downloads
config.bind("xd", "download-clear")

# ctrl instead of alt for navigation by tab number
config.bind("<Ctrl-1>", "tab-focus 1")
config.bind("<Ctrl-2>", "tab-focus 2")
config.bind("<Ctrl-3>", "tab-focus 3")
config.bind("<Ctrl-4>", "tab-focus 4")
config.bind("<Ctrl-5>", "tab-focus 5")
config.bind("<Ctrl-6>", "tab-focus 6")
config.bind("<Ctrl-7>", "tab-focus 7")
config.bind("<Ctrl-8>", "tab-focus 8")
config.bind("<Ctrl-9>", "tab-focus -1")

# tab navigation by direction
config.bind("<Ctrl-Left>", "tab-prev")
config.bind("<Ctrl-Right>", "tab-next")

# history using arrow keys
config.bind("<Shift-Left>", "back")
config.bind("<Shift-Right>", "forward")

# fix clipboard behaviour
config.bind("<Ctrl-v>", 'insert-text {primary}', mode='insert')
c.content.javascript.can_access_clipboard = True

#
# stylesheets and other per-domain settings
#
#

# discord
# with config.pattern("discord.com") as p:
#     p.content.user_stylesheets = "~/dotfiles/misc/css/discord.css"

# element
# with config.pattern("app.element.io") as p:
#     p.content.user_stylesheets = "~/dotfiles/misc/css/element.css"
