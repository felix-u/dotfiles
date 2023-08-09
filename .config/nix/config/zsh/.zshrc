#!/usr/bin/env zsh

setopt extendedglob
setopt globdots
setopt sharehistory

export PAGER=(less -FIRX)
export MANPAGER="$PAGER"

if test -z "${XDG_RUNTIME_DIR}"; then
    export XDG_RUNTIME_DIR=/tmp/$(id -u)-runtime-dir
    if ! test -d "${XDG_RUNTIME_DIR}"; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi
fi

# pwd
PROMPT="%(?.%F{normal}.%F{red})%(!.#.%${PROMPTCHAR})%f "

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
#
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

_zsh_cli_fg() { fg; }
zle -N _zsh_cli_fg
bindkey '^Z' _zsh_cli_fg

# _clear_and_pwd () {
#     clear
#     pwd
#     zle redisplay
# }
# zle -N _clear_and_pwd
# bindkey "^L" _clear_and_pwd

# Fix home and end keys.

bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi

any-nix-shell zsh --info-right | source /dev/stdin
