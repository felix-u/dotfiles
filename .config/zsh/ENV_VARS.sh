export XRESOURCES="$HOME/.Xresources"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$XDG_CONFIG_HOME/zsh/scripts-in-path:$PATH"
export PKG_CONFIG_PATH="/usr/include"
export BAT_THEME="ansi"

# Defaults
export TERMINAL='foot'
export TERM="$TERMINAL"
export TERM_ITALICS=true
export EDITOR='nvim'
export VISUAL="nvim"
export READER='zathura'
export VIDEO='mpv'
export IMAGE='imv'
export WM='sway'
export PAGER='less'
export MANPAGER=$PAGER
export BROWSER="qutebrowser"

# LESS colours
export LESS_TERMCAP_mb=$'\e[1;36m'
export LESS_TERMCAP_md=$'\e[1;34m'
export LESS_TERMCAP_me=$'\e[32m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;31m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;33m'

export QT_QPA_PLATFORMTHEME="qt5ct"

export STARSHIP_CONFIG=~/.config/starship/starship.toml

# nnn file manager
export NNN_FIFO='/tmp/nnn.fifo'
export NNN_TMPFILE="/tmp/nnn"
export NNN_COLORS='4444'
export NNN_TRASH=1
export NNN_PLUG="v:imgview;d:dragdrop;r:imgresize;p:preview-tui;w:waypaper"
export NNN_OPTS="C"
export NNN_BMS="r:$HOME/Desktop/recordings;\
s:$HOME/Pictures/screenshots;\
h:$HOME;\
d:$HOME/dotfiles;\
m:/mnt;\
u:$HOME/uni/2022/spring;"

# xdg-open and xdg-mime associations
handlr set image/png imv.desktop
handlr set image/jpeg imv.desktop
handlr set image/ppm imv.desktop

# make cd great again
cdpath=( $HOME/ uni/2022/spring)

# XDG environment variables
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_CURRENT_DESKTOP=sway

# git config
git config --global core.editor "nvim"
