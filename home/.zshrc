#!/usr/bin/env zsh

SAVEHIST=10000
HISTSIZE=$SAVEHIST
HISTFILE="$XDG_CACHE_HOME/zsh_history"

setopt autocd
setopt extendedglob
setopt globdots
setopt sharehistory

source /home/felix/.config/sh/env.sh
source /home/felix/.config/sh/aliases_funcs.sh

# if [[ $(tty) == /dev/tty* ]];then
#     printf "
# \033]P0$CLR_BG 
# \033]P1$CLR_01 
# \033]P2$CLR_02 
# \033]P3$CLR_03 
# \033]P4$CLR_04 
# \033]P5$CLR_05 
# \033]P6$CLR_06 
# \033]P7$CLR_FG 
# \033]P8$CLR_08 
# \033]P9$CLR_09 
# \033]PA$CLR_10 
# \033]PB$CLR_11 
# \033]PC$CLR_12 
# \033]PD$CLR_13 
# \033]PE$CLR_14 
# \033]PF$CLR_FG 
# "
# fi

export PAGER=(less -FIR)
export MANPAGER="$PAGER"

if test -z "${XDG_RUNTIME_DIR}"; then
    export XDG_RUNTIME_DIR=/tmp/$(id -u)-runtime-dir
    if ! test -d "${XDG_RUNTIME_DIR}"; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi
fi

pwd
PROMPT="%(?.%F{normal}.%F{red})%(!.#.${PROMPTCHAR})%f "

# Auto/tab complete ---
autoload -U compinit
compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots) # Include hidden files
# History awareness
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
# Case-insensitive autocomplete
zstyle ':completion:*' matcher-list '' \
  'm:{a-z\-}={A-Z\_}' \
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
  'r:|?=** m:{a-z\-}={A-Z\_}'
# ---

# Vim keys ---
bindkey -v
export KEYTIMEOUT=1
export VI_MODE_SET_CURSOR=true
bindkey "^?" backward-delete-char

bindkey -M viins '^P' vi-cmd-mode
bindkey -M vicmd '^P' vi-insert-mode

# Block cursor, always:
function _set_cursor() {
    if [[ $TMUX = '' ]]; then
      echo -ne $1
    else
      echo -ne "\ePtmux;\e\e$1\e\\"
    fi
}
function _set_block_cursor() { _set_cursor '\e[2 q' }
precmd_functions+=(_set_block_cursor) #
# ---

# Emit escape sequence to spawn new terminals in current working directory
_urlencode() {
	local length="${#1}"
	for (( i = 0; i < length; i++ )); do
		local c="${1:$i:1}"
		case $c in
			%) printf '%%%02X' "'$c" ;;
			*) printf "%s" "$c" ;;
		esac
	done
}
osc7_cwd() {
	printf '\e]7;file://%s%s\e\\' "$HOSTNAME" "$(_urlencode "$PWD")"
}
autoload -Uz add-zsh-hook
add-zsh-hook -Uz chpwd osc7_cwd

cdpath=($HOME)

_zsh_cli_fg() { fg; }
zle -N _zsh_cli_fg
bindkey '^Z' _zsh_cli_fg

_clear_and_pwd () {
    clear
    pwd
    zle redisplay
}
zle -N _clear_and_pwd
bindkey "^L" _clear_and_pwd

# Fix home and end keys.
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

bindkey -s "^G" 'fcd^M'

source "$XDG_CONFIG_HOME"/sh/zsh-fzf-history-search/zsh-fzf-history-search.plugin.zsh

if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi

any-nix-shell zsh --info-right | source /dev/stdin
