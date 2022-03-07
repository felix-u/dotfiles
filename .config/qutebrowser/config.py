import subprocess

# try to calm down LSP
config = config
c = c

config.set("colors.webpage.darkmode.enabled", False)
config.set("fonts.default_size", "12pt")
config.load_autoconfig(False)

config.set("zoom.default", "100%")
config.set("qt.highdpi", True)

# open videos with mpv
config.bind(",m", 'hint links spawn mpv {hint-url}')

# clear downloads
config.bind("xd", "download-clear")

colours = {
        'background': '#002b36',
        'foreground': '#93a1a1',
        'color0': '#073642',
        'color1': '#dc322f',
        'color2': '#859900',
        'color3': '#b58900',
        'color4': '#268bd2',
        'color5': '#6c71c4',
        'color6': '#2aa198',
        'color7': '#657b83',
        'color8': '#224750',
        'color9': '#dc322f',
        'color10': '#859900',
        'color11': '#b58900',
        'color12': '#268bd2',
        'color13': '#6c71c4',
        'color14': '#2aa198',
        'color15': '#839496',
        'fontmono': 'IosevkaCustom'
        }

config.set("fonts.default_family", colours['fontmono'])

# hide some stuff
config.set("tabs.show", "never")
config.set("statusbar.show", "never")

## *.Background color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.bg = colours['background']

## Bottom border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.border.bottom = colours['background']

## Top border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.category.border.top = colours['background']

## *.Foreground color of completion widget category headers.
## Type: QtColor
c.colors.completion.category.fg = colours['foreground']

## *.Background color of the completion widget for even rows.
## Type: QssColor
c.colors.completion.even.bg = colours['color0']

## *.Background color of the completion widget for odd rows.
## Type: QssColor
c.colors.completion.odd.bg = colours['color0']

## Text color of the completion widget.
## Type: QtColor
c.colors.completion.fg = colours['color15']

## *.Background color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.bg = colours['color8']

## Bottom border color of the selected completion item.
## Type: QssColor
c.colors.completion.item.selected.border.bottom = colours['color8']

## Top border color of the completion widget category headers.
## Type: QssColor
c.colors.completion.item.selected.border.top = colours['color8']

## *.Foreground color of the selected completion item.
## Type: QtColor
c.colors.completion.item.selected.fg = colours['foreground']

## *.Foreground color of the matched text in the completion.
## Type: QssColor
c.colors.completion.match.fg = colours['color3']

## Color of the scrollbar in completion view
## Type: QssColor
c.colors.completion.scrollbar.bg = colours['color0']

## Color of the scrollbar handle in completion view.
## Type: QssColor
c.colors.completion.scrollbar.fg = colours['foreground']

## *.Background color for the download bar.
## Type: QssColor
c.colors.downloads.bar.bg = colours['background']

## *.Background color for downloads with errors.
## Type: QtColor
c.colors.downloads.error.bg = colours['color1']

## *.Foreground color for downloads with errors.
## Type: QtColor
c.colors.downloads.error.fg = colours['foreground']

## Color gradient stop for download *.backgrounds.
## Type: QtColor
c.colors.downloads.stop.bg = colours['color5']

## Color gradient interpolation system for download *.backgrounds.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.downloads.system.bg = 'none'

## Type: QssColor
c.colors.hints.bg = colours['color3']

## Font color for hints.
## Type: QssColor
c.colors.hints.fg = colours['background']

## Font color for the matched part of hints.
## Type: QssColor
c.colors.hints.match.fg = colours['color4']

## *.Background color of the keyhint widget.
## Type: QssColor
c.colors.keyhint.bg = colours['color0']

## Text color for the keyhint widget.
## Type: QssColor
c.colors.keyhint.fg = colours['foreground']

## Highlight color for keys to complete the current keychain.
## Type: QssColor
c.colors.keyhint.suffix.fg = colours['color3']

## *.Background color of an error message.
## Type: QssColor
c.colors.messages.error.bg = colours['color1']

## Border color of an error message.
## Type: QssColor
c.colors.messages.error.border = colours['color1']

## *.Foreground color of an error message.
## Type: QssColor
c.colors.messages.error.fg = colours['foreground']

## *.Background color of an info message.
## Type: QssColor
c.colors.messages.info.bg = colours['color6']

## Border color of an info message.
## Type: QssColor
c.colors.messages.info.border = colours['color6']

## *.Foreground color an info message.
## Type: QssColor
c.colors.messages.info.fg = colours['foreground']

## *.Background color of a warning message.
## Type: QssColor
c.colors.messages.warning.bg = colours['color1']

## Border color of a warning message.
## Type: QssColor
c.colors.messages.warning.border = colours['color1']

## *.Foreground color a warning message.
## Type: QssColor
c.colors.messages.warning.fg = colours['foreground']

## *.Background color for prompts.
## Type: QssColor
c.colors.prompts.bg = colours['color8']

# ## Border used around UI elements in prompts.
# ## Type: String
c.colors.prompts.border = '1px solid ' + colours['background']

## *.Foreground color for prompts.
## Type: QssColor
c.colors.prompts.fg = colours['foreground']

## *.Background color for the selected item in filename prompts.
## Type: QssColor
c.colors.prompts.selected.bg = colours['color8']

## *.Background color of the statusbar in caret mode.
## Type: QssColor
c.colors.statusbar.caret.bg = colours['color5']

## *.Foreground color of the statusbar in caret mode.
## Type: QssColor
c.colors.statusbar.caret.fg = colours['foreground']

## *.Background color of the statusbar in caret mode with a selection.
## Type: QssColor
c.colors.statusbar.caret.selection.bg = colours['color5']

## *.Foreground color of the statusbar in caret mode with a selection.
## Type: QssColor
c.colors.statusbar.caret.selection.fg = colours['foreground']

## *.Background color of the statusbar in command mode.
## Type: QssColor
c.colors.statusbar.command.bg = colours['color8']

## *.Foreground color of the statusbar in command mode.
## Type: QssColor
c.colors.statusbar.command.fg = colours['foreground']

## *.Background color of the statusbar in private browsing + command mode.
## Type: QssColor
c.colors.statusbar.command.private.bg = colours['color8']

## *.Foreground color of the statusbar in private browsing + command mode.
## Type: QssColor
c.colors.statusbar.command.private.fg = colours['foreground']

## *.Background color of the statusbar in insert mode.
## Type: QssColor
c.colors.statusbar.insert.bg = colours['color2']

## *.Foreground color of the statusbar in insert mode.
## Type: QssColor
c.colors.statusbar.insert.fg = colours['color0']

## *.Background color of the statusbar.
## Type: QssColor
c.colors.statusbar.normal.bg = colours['background']

## *.Foreground color of the statusbar.
## Type: QssColor
c.colors.statusbar.normal.fg = colours['foreground']

## *.Background color of the statusbar in passthrough mode.
## Type: QssColor
c.colors.statusbar.passthrough.bg = colours['color4']

## *.Foreground color of the statusbar in passthrough mode.
## Type: QssColor
c.colors.statusbar.passthrough.fg = colours['foreground']

## *.Background color of the statusbar in private browsing mode.
## Type: QssColor
c.colors.statusbar.private.bg = colours['color8']

## *.Foreground color of the statusbar in private browsing mode.
## Type: QssColor
c.colors.statusbar.private.fg = colours['foreground']

## *.Background color of the progress bar.
## Type: QssColor
c.colors.statusbar.progress.bg = colours['foreground']

## *.Foreground color of the URL in the statusbar on error.
## Type: QssColor
c.colors.statusbar.url.error.fg = colours['color1']

## Default *.foreground color of the URL in the statusbar.
## Type: QssColor
c.colors.statusbar.url.fg = colours['foreground']

## *.Foreground color of the URL in the statusbar for hovered links.
## Type: QssColor
c.colors.statusbar.url.hover.fg = colours['color6']

## *.Foreground color of the URL in the statusbar on successful load
## (http).
## Type: QssColor
c.colors.statusbar.url.success.http.fg = colours['foreground']

## *.Foreground color of the URL in the statusbar on successful load
## (https).
## Type: QssColor
c.colors.statusbar.url.success.https.fg = colours['color2']

## *.Foreground color of the URL in the statusbar when there's a warning.
## Type: QssColor
c.colors.statusbar.url.warn.fg = colours['color1']

## *.Background color of the tab bar.
## Type: QtColor
c.colors.tabs.bar.bg = colours['color8']

## *.Background color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.bg = colours['color8']

## *.Foreground color of unselected even tabs.
## Type: QtColor
c.colors.tabs.even.fg = colours['foreground']

## Color for the tab indicator on errors.
## Type: QtColor
c.colors.tabs.indicator.error = colours['color1']

## Color gradient interpolation system for the tab indicator.
## Type: ColorSystem
## Valid values:
##   - rgb: Interpolate in the RGB color system.
##   - hsv: Interpolate in the HSV color system.
##   - hsl: Interpolate in the HSL color system.
##   - none: Don't show a gradient.
c.colors.tabs.indicator.system = 'none'

## *.Background color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.bg = colours['color8']

## *.Foreground color of unselected odd tabs.
## Type: QtColor
c.colors.tabs.odd.fg = colours['foreground']

# ## *.Background color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.bg = colours['background']

# ## *.Foreground color of selected even tabs.
# ## Type: QtColor
c.colors.tabs.selected.even.fg = colours['foreground']

# ## *.Background color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.bg = colours['background']

# ## *.Foreground color of selected odd tabs.
# ## Type: QtColor
c.colors.tabs.selected.odd.fg = colours['foreground']

## *.Background color for webpages if unset (or empty to use the theme's
## color)
## Type: QtColor
c.colors.webpage.bg = colours['background']
