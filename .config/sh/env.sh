#!/usr/bin/env sh

export AGENDAFILE="$HOME/uni/2023/spring/agenda"
export AMD_VULKAN_ICD="RADV"
export BROWSER="firefox"
export CC="zig cc"
export DOTFILES="$HOME/dotfiles"
export DOTSCONF="$DOTFILES/.config"
export EDITOR="nvim"
export FILES="io.elementary.files --new-window"
export FZF_DEFAULT_COMMAND="fd"
export FZF_DEFAULT_OPTS="--color=16"
export GREP_COLORS="ms=1;97"
export IMAGE="imv"
export LESS_TERMCAP_mb=$'\e\[1\;36m'
export LESS_TERMCAP_md=$'\e\[1\;1m'
export LESS_TERMCAP_me=$'\e\[0m'
export LESS_TERMCAP_se=$'\e\[0m'
export LESS_TERMCAP_so=$'\e\[7m'
export LESS_TERMCAP_ue=$'\e\[0m'
export LESS_TERMCAP_us=$'\e\[1\;4m'
export NIXOS_OZONE_WL=1
export NNN_COLORS="4444"
export NNN_FIFO="/tmp/nnn.fifo"
export NNN_OPTS="C"
export NNN_TMPFILE="/tmp/nnn"
export NNN_TRASH=1
export PAGER="less -FIRX"
export PATH="$HOME/.local/bin/:$XDG_CONFIG_HOME/sh/scripts-in-path/:$PATH"
export PKG_CONFIG_PATH="/usr/include"
export PROMPTCHAR="%%" #"λ"
export PS1="$PROMPTCHAR "
export QT_QPA_PLATFORMTHEME="qt5ct"
export READER="zathura"
export RUSTC_WRAPPER="sccache"
export SDL_VIDEODRIVER="wayland"
export STARSHIP_CONFIG="$XDG_CONFIG_HOME/starship/starship.toml"
export TERMINAL="foot"
export TERM_ITALICS=true
export VIDEO="mpv"
export VISUAL="nvim"
export WM="sway"
export XCURSOR_SIZE=24
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_CURRENT_DESKTOP=sway
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"
export XRESOURCES="$HOME/.Xresources"

# Depends on the above.

export MANPAGER=$PAGER
