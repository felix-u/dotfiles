#!/usr/bin/env zsh

# History configuration
HISTFILE="$XDG_CACHE_HOME"/zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY

# Prompt - starship for now
# PROMPT="%B%F{cyan}%3~"$'\n'"%(?.%F{blue}.%F{magenta})%(!.%F{red}#.λ) %f%b"
eval "$(starship init zsh)"

# Environment variables
source "$XDG_CONFIG_HOME"/sh/env.sh

# Aliases and functions
source "$XDG_CONFIG_HOME"/sh/aliases_funcs.sh

# Auto/tab complete ---
# Menu style
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
# # Change cursor, with tmux support
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

# cd to given dir if no command given
setopt auto_cd

# Show completion menu on successive tab press
setopt automenu

# More globbing features
setopt extendedglob
setopt globdots

# Home and End keys should work as expected
bindkey  "^[[H"   beginning-of-line
bindkey  "^[[F"   end-of-line

# Fish-like autosuggestions
source "$XDG_CONFIG_HOME"/sh/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
# # ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=black"
bindkey '^n' autosuggest-accept

# History-aware autocomplete
source "$XDG_CONFIG_HOME"/sh/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="fg=blue"
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND="fg=magenta"

# Automatic syntax pairs
source "$XDG_CONFIG_HOME"/sh/zsh/zsh-autopair/autopair.zsh

# # Plugin: zsh-synax-highlighting
# source \
#     "$XDG_CONFIG_HOME"/sh/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
# ZSH_HIGHLIGHT_HIGHLIGHTERS+=(brackets)
# typeset -A ZSH_HIGHLIGHT_STYLES
# ZSH_HIGHLIGHT_STYLES[bracket-level-1]='fg=magenta'
# ZSH_HIGHLIGHT_STYLES[bracket-level-2]='fg=magenta'
# ZSH_HIGHLIGHT_STYLES[bracket-level-3]='fg=magenta'
# ZSH_HIGHLIGHT_STYLES[bracket-level-4]='fg=magenta'
# ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=blue'
# ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=cyan'
# ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=cyan'
# ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=fg,bold'

# Nix
if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi

