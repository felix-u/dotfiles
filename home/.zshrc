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

NEWLINE=$'\n'
PS1="%B%F{cyan}%5~%f${NEWLINE}%(?.%F{normal}.%F{red})%(!.#.${PROMPTCHAR})%f%b "

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

# CTRL-Z brings to foreground the background process.
_zsh_cli_fg() { fg; }
zle -N _zsh_cli_fg
bindkey '^Z' _zsh_cli_fg

# Home and End keys should work as expected
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

bindkey '^n' autosuggest-accept

bindkey -s "^G" 'fcd^M'

# Nix
if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi
any-nix-shell zsh --info-right | source /dev/stdin
