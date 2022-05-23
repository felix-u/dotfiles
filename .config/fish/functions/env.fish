#!/usr/bin/env fish

set -Ux XRESOURCES "$HOME/.Xresources"
set -Ux DOTFILES "$HOME/dotfiles"
set -Ux DOTSCONF "$DOTFILES/.config"

set -Ux PATH "$HOME/.local/bin:$PATH"
set -Ux PATH "$XDG_CONFIG_HOME/zsh/scripts-in-path:$PATH"
set -Ux PKG_CONFIG_PATH "/usr/include"
set -Ux BAT_THEME "ansi"

# Defaults
set -Ux TERMINAL 'foot'
set -Ux TERM_ITALICS true
set -Ux EDITOR 'nvim'
set -Ux VISUAL "nvim"
set -Ux READER 'zathura'
set -Ux VIDEO 'mpv'
set -Ux IMAGE 'imv'
set -Ux WM 'sway'
set -Ux PAGER 'less'
set -Ux MANPAGER $PAGER
set -Ux BROWSER "qutebrowser"

# LESS colours
set -Ux LESS_TERMCAP_mb '\e[1;36m'
set -Ux LESS_TERMCAP_md '\e[1;34m'
set -Ux LESS_TERMCAP_me '\e[32m'
set -Ux LESS_TERMCAP_se '\e[0m'
set -Ux LESS_TERMCAP_so '\e[01;31m'
set -Ux LESS_TERMCAP_ue '\e[0m'
set -Ux LESS_TERMCAP_us '\e[1;36m'

set -Ux QT_QPA_PLATFORMTHEME "qt5ct"

set -Ux STARSHIP_CONFIG ~/.config/starship/starship.toml

# nnn file manager
set -Ux NNN_FIFO '/tmp/nnn.fifo'
set -Ux NNN_TMPFILE "/tmp/nnn"
set -Ux NNN_COLORS '4444'
set -Ux NNN_TRASH 1
set -Ux NNN_OPTS "C"

# xdg-open and xdg-mime associations
handlr set image/png imv.desktop
handlr set image/jpeg imv.desktop
handlr set image/ppm imv.desktop

# make cd great again
set cdpath $HOME

# XDG environment variables
set -Ux XDG_CONFIG_HOME "$HOME/.config"
set -Ux XDG_CACHE_HOME "$HOME/.cache"
set -Ux XDG_DATA_HOME "$HOME/.local/share"
set -Ux XDG_STATE_HOME "$HOME/.local/state"
set -Ux XDG_CURRENT_DESKTOP sway

# git config
git config --global core.editor $EDITOR
