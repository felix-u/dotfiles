#!/usr/bin/env zsh

SAVEHIST=10000

source /home/felix/.config/sh/env.sh
source /home/felix/.config/sh/aliases_funcs.sh

NEWLINE=$'\n'
PROMPT="%B%F{cyan}%5~%f${NEWLINE}%(?.%F{normal}.%F{red})%(!.#.${PROMPTCHAR})%f%b "

export LESS_TERMCAP_mb=$'\e\[1\;36m'
export LESS_TERMCAP_md=$'\e\[1\;1m'
export LESS_TERMCAP_me=$'\e\[0m'
export LESS_TERMCAP_se=$'\e\[0m'
export LESS_TERMCAP_so=$'\e\[0\;37m'
export LESS_TERMCAP_ue=$'\e\[0m'
export LESS_TERMCAP_us=$'\e\[1\;4m'

export PAGER=(less -FIRX)
export MANPAGER="nvim -c ASMANPAGER"

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

    # # Change cursor, with tmux support ---
    # function _set_cursor() {
    #     if [[ $TMUX = '' ]]; then
    #       echo -ne $1
    #     else
    #       echo -ne "\ePtmux;\e\e$1\e\\"
    #     fi
    # }
    # function _set_block_cursor() { _set_cursor '\e[2 q' }
    # function _set_beam_cursor() { _set_cursor '\e[6 q' }
    # function zle-keymap-select {
    #   if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
    #       _set_block_cursor
    #   else
    #       _set_beam_cursor
    #   fi
    # }
    # zle -N zle-keymap-select
    # # ensure beam cursor when starting new terminal
    # precmd_functions+=(_set_beam_cursor) #
    # # ensure insert mode and beam cursor when exiting vim
    # zle-line-init() { zle -K viins; _set_beam_cursor }
    # # ---

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

# Home and End keys should work as expected
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

bindkey '^n' autosuggest-accept

source "$XDG_CONFIG_HOME"/sh/zsh/fzf-tab/fzf-tab.plugin.zsh
disable-fzf-tab
bindkey '^t' toggle-fzf-tab

# # History-aware autocomplete
# source /home/felix/.config/sh/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
# bindkey '^[[A' history-substring-search-up
# bindkey '^[[B' history-substring-search-down
# HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="fg=blue"
# HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND="fg=magenta"
source "$XDG_CONFIG_HOME"/sh/zsh/zsh-fzf-history-search/zsh-fzf-history-search.zsh
bindkey -s "^G" 'fcd^M'

# Automatic syntax pairs
source /home/felix/.config/sh/zsh/zsh-autopair/autopair.zsh

# Nix
if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi
