set -x XRESOURCES "$HOME/.Xresources"
set -x DOTFILES "$HOME/dotfiles"
set -x DOTSCONF "$DOTFILES/.config"

set -x PATH "$HOME/.local/bin:$PATH"
set -x PATH "$XDG_CONFIG_HOME/zsh/scripts-in-path:$PATH"
set -x PKG_CONFIG_PATH "/usr/include"
set -x BAT_THEME "ansi"

# Defaults
set -x TERMINAL 'foot'
set -x TERM_ITALICS true
set -x EDITOR 'nvim'
set -x VISUAL "nvim"
set -x READER 'zathura'
set -x VIDEO 'mpv'
set -x IMAGE 'imv'
set -x WM 'sway'
set -x PAGER 'less'
set -x MANPAGER $PAGER
set -x BROWSER "qutebrowser"

# LESS colours
set -x LESS_TERMCAP_mb '\e[1;36m'
set -x LESS_TERMCAP_md '\e[1;34m'
set -x LESS_TERMCAP_me '\e[32m'
set -x LESS_TERMCAP_se '\e[0m'
set -x LESS_TERMCAP_so '\e[01;31m'
set -x LESS_TERMCAP_ue '\e[0m'
set -x LESS_TERMCAP_us '\e[1;36m'

set -x QT_QPA_PLATFORMTHEME "qt5ct"

set -x STARSHIP_CONFIG ~/.config/starship/starship.toml

# nnn file manager
set -x NNN_FIFO '/tmp/nnn.fifo'
set -x NNN_TMPFILE "/tmp/nnn"
set -x NNN_COLORS '4444'
set -x NNN_TRASH 1
set -x NNN_OPTS "C"

# xdg-open and xdg-mime associations
handlr set image/png imv.desktop
handlr set image/jpeg imv.desktop
handlr set image/ppm imv.desktop

# make cd great again
set cdpath $HOME

# XDG environment variables
set -x XDG_CONFIG_HOME "$HOME/.config"
set -x XDG_CACHE_HOME "$HOME/.cache"
set -x XDG_DATA_HOME "$HOME/.local/share"
set -x XDG_STATE_HOME "$HOME/.local/state"
set -x XDG_CURRENT_DESKTOP sway

# git config
git config --global core.editor $EDITOR
