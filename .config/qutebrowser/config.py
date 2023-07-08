import subprocess

config.set("colors.webpage.darkmode.enabled", False)
config.set("fonts.default_size", "12pt")
config.load_autoconfig(False)

config.set("zoom.default", "100%")
config.set("qt.highdpi", True)

term = 'foot'
editor = 'nvim'
theme = {
    'background': '#ffffff',
    'foreground': '#000000',
    'color0': '#efefef',
    'color1': '#a0342f',
    'color2': '#065905',
    'color3': '#999950',
    'color4': '#007ed6',
    'color5': '#8888cc',
    'color6': '#57a8a8',
    'color7': '#777777',
    'color8': '#c0c0c0',
    'color9': '#a0342f',
    'color10': '#065905',
    'color11': '#999950',
    'color12': '#007ed6',
    'color13': '#8888cc',
    'color14': '#57a8a8',
    'color15': '#000000',
    'fontmono': 'Iosevka',
    'fontsans': 'Inter',
}

config.set("fonts.default_family", theme['fontsans'])


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

# # completion menu 
# c.colors.completion.category.bg = theme['color0']
# c.colors.completion.category.fg = theme['foreground']
# c.colors.completion.category.border.bottom = theme['background']
# c.colors.completion.category.border.top = theme['background']
# c.colors.completion.even.bg = theme['background']
# c.colors.completion.odd.bg = theme['background']
# c.colors.completion.fg = theme['color15']
# c.colors.completion.item.selected.bg = theme['color8']
# c.colors.completion.item.selected.border.bottom = theme['color8']
# c.colors.completion.item.selected.border.top = theme['color8']
# c.colors.completion.item.selected.fg = theme['foreground']
# c.colors.completion.match.fg = theme['color2']
# c.colors.completion.scrollbar.bg = theme['color0']
# c.colors.completion.scrollbar.fg = theme['foreground']

# # context menu (right click) 
# c.colors.contextmenu.disabled.bg = theme['color0']
# c.colors.contextmenu.disabled.fg = theme['color7']
# c.colors.contextmenu.menu.bg = theme['color0']
# c.colors.contextmenu.menu.fg = theme['color15']
# c.colors.contextmenu.selected.bg = theme['color8']
# c.colors.contextmenu.selected.fg = theme['foreground']

# # download bar 
# c.colors.downloads.bar.bg = theme['color0']
# c.colors.downloads.error.bg = theme['color1']
# c.colors.downloads.error.fg = theme['background']
# c.colors.downloads.start.bg = theme['background']
# c.colors.downloads.start.fg = theme['foreground']
# c.colors.downloads.stop.bg = theme['background']
# c.colors.downloads.stop.fg = theme['foreground']
# c.colors.downloads.system.bg = 'none'
# c.colors.downloads.system.fg = 'none'

# # hints (labels on links) 
# c.colors.hints.bg = theme['color3']
# c.colors.hints.fg = theme['background']
# c.colors.hints.match.fg = theme['background']
# c.colors.keyhint.bg = theme['color0']
# c.colors.keyhint.fg = theme['foreground']
# c.colors.keyhint.suffix.fg = theme['color5']
# c.hints.border = '0px solid #000000'
# c.hints.mode = 'number'

# # info and error messages 
# c.colors.messages.error.bg = theme['color1']
# c.colors.messages.error.border = theme['color1']
# c.colors.messages.error.fg = theme['background']
# c.colors.messages.info.bg = theme['color0']
# c.colors.messages.info.border = theme['color0']
# c.colors.messages.info.fg = theme['foreground']
# c.colors.messages.warning.bg = theme['color3']
# c.colors.messages.warning.border = theme['color3']
# c.colors.messages.warning.fg = theme['background']

# # prompts 
# c.colors.prompts.bg = theme['color0']
# c.colors.prompts.border = theme['color0']
# c.colors.prompts.fg = theme['color15']
# c.colors.prompts.selected.bg = theme['color8']
# c.colors.prompts.selected.fg = theme['foreground']

# statusbar 
c.colors.statusbar.caret.bg = theme['color6']
c.colors.statusbar.caret.fg = theme['background']
c.colors.statusbar.caret.selection.bg = theme['color4']
c.colors.statusbar.caret.selection.fg = theme['background']
c.colors.statusbar.command.bg = theme['foreground']
c.colors.statusbar.command.fg = theme['background']
c.colors.statusbar.command.private.bg = theme['color5']
c.colors.statusbar.command.private.fg = theme['background']
c.colors.statusbar.insert.bg = theme['color2']
c.colors.statusbar.insert.fg = theme['background']
c.colors.statusbar.normal.bg = theme['color0']
c.colors.statusbar.normal.fg = theme['foreground']
c.colors.statusbar.passthrough.bg = theme['color0']
c.colors.statusbar.passthrough.fg = theme['color4']
c.colors.statusbar.private.bg = theme['color0']
c.colors.statusbar.private.fg = theme['color5']
c.colors.statusbar.progress.bg = theme['color8']
c.colors.statusbar.url.error.fg = theme['color1']
c.colors.statusbar.url.fg = theme['foreground']
c.colors.statusbar.url.hover.fg = theme['foreground']
c.colors.statusbar.url.success.http.fg = theme['color3']
c.colors.statusbar.url.success.https.fg = theme['color2']
c.colors.statusbar.url.warn.fg = theme['color1']
c.statusbar.padding = {"bottom": 6, "left": 4, "right": 4, "top": 6}
c.statusbar.show = "always"

# tabs 
c.colors.tabs.bar.bg = theme['color0']
c.colors.tabs.even.bg = theme['color0']
c.colors.tabs.even.fg = theme['foreground']
c.colors.tabs.odd.bg = theme['color0']
c.colors.tabs.odd.fg = theme['foreground']
c.colors.tabs.pinned.even.bg = theme['color0']
c.colors.tabs.pinned.even.fg = theme['foreground']
c.colors.tabs.pinned.odd.bg = theme['color0']
c.colors.tabs.pinned.odd.fg = theme['foreground']
c.colors.tabs.selected.even.bg = theme['foreground']
c.colors.tabs.selected.even.fg = theme['background']
c.colors.tabs.selected.odd.bg = theme['foreground']
c.colors.tabs.selected.odd.fg = theme['background']
c.colors.tabs.pinned.selected.even.bg = theme['foreground']
c.colors.tabs.pinned.selected.even.fg = theme['background']
c.colors.tabs.pinned.selected.odd.bg = theme['foreground']
c.colors.tabs.pinned.selected.odd.fg = theme['background']
# c.colors.tabs.indicator.error = theme['color1']
# c.colors.tabs.indicator.start = theme['color4']
# c.colors.tabs.indicator.stop = theme['color4']
# c.colors.tabs.indicator.system = 'none'

c.tabs.show = "multiple"
c.tabs.indicator.padding = {"bottom": 4, "left": 1, "right": 8, "top": 4}
c.tabs.last_close = 'close'
c.tabs.padding = {"bottom": 6, "left": 9, "right": 9, "top": 6}
c.tabs.select_on_remove = 'prev'

# webpage 
c.colors.webpage.bg = theme['background']
c.colors.webpage.preferred_color_scheme = 'light'

# content settings 
c.content.blocking.enabled = True
c.content.blocking.method = 'both'
c.content.cookies.accept = "no-3rdparty"

# downloads 
c.downloads.location.remember = False
c.downloads.position = 'bottom'

# # fileselect 
# c.fileselect.folder.command = [term, "-e", "nnn",  "-p", "{}"]
# c.fileselect.handler = 'external'
# c.fileselect.multiple_files.command = [term, "-e", "nnn",  "-p", "{}"]
# c.fileselect.single_file.command = [term, "-e", "nnn",  "-p", "{}"]

# input behaviour 
c.input.insert_mode.auto_load = True

# new instance behaviour 
c.new_instance_open_target = 'window'

# url 
c.url.default_page = "https://google.com"
c.url.searchengines = {"DEFAULT": "https://google.com/search?q={}"}
c.url.start_pages = "https://google.com"

# editor 
c.editor.command = [term, "-e", editor, "{file}", "-c", "{line}"]

# Bindings

# open videos with mpv
config.bind(",m", 'hint links spawn mpv {hint-url}')

# fix copying on wayland (not working)
config.bind("<Ctrl-Shift-P>", 'spawn wl-paste')

# clear downloads
config.bind("xd", "download-clear")

# toggle statusbar
config.bind("gss", "set statusbar.show always")
config.bind("gsh", "set statusbar.show in-mode")

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
c.content.javascript.can_access_clipboard = True
