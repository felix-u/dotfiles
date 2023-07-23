#!/usr/bin/env sh

export FONT_MONO="Iosevka"
export FONT_SANS="Inter"
export FONT_SERIF="EBGaramond12"

export CLR_FG="000000"
export CLR_BG="ffffff"
export CLR_00="efefef"
export CLR_08="c0c0c0"
export CLR_01="a0342f"
export CLR_09="a0342f"
export CLR_02="065905"
export CLR_10="065905"
export CLR_03="999950"
export CLR_11="999950"
export CLR_04="007ed6"
export CLR_12="007ed6"
export CLR_05="8888cc"
export CLR_13="8888cc"
export CLR_06="57a8a8"
export CLR_14="57a8a8"
export CLR_07="777777"
export CLR_15="000000"

export CLR_FG_ALT="000000"
export CLR_BG_ALT="fdffea"
export CLR_00_ALT="eeee9e"
export CLR_08_ALT="c1c270"
export CLR_15_ALT="000000"

export AGENDAFILE="$HOME/uni/2023/autumn/agenda"
export AMD_VULKAN_ICD="RADV"
export BROWSER="firefox"
export CC="zig cc"
export DOTFILES="$HOME/dotfiles"
export DOTSCONF="$DOTFILES/.config"
export EDITOR="nvim"
export FILES="io.elementary.files --new-window"
export FZF_DEFAULT_COMMAND="fd"
export FZF_DEFAULT_OPTS="--color=16"
export GFORTHHIST="$XDG_CACHE_HOME"/.gforth-history
export GREP_COLORS="ms=1;97"
export IMAGE="imv"
export LESS_TERMCAP_mb=$'\e[1;36m'
export LESS_TERMCAP_md=$'\e[1;1m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[7m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4m'
export MANWIDTH=80
export MOZ_ENABLE_WAYLAND="1"
export NIXOS_OZONE_WL=1
export PAGER="less"
export PATH="$HOME/.local/bin/:$XDG_CONFIG_HOME/sh/scripts-in-path/:$PATH"
export PKG_CONFIG_PATH="/usr/include"
export PROMPTCHAR="%%" #"λ"
export PS1="$PROMPTCHAR "
export QT_QPA_PLATFORM="wayland-egl"
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
