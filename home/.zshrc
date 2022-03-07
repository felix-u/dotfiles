autoload -U colors && colors

# History in cache directory:
HISTFILE=~/.cache/zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY

# Starship prompt
eval "$(starship init zsh)"

# Environment variables
source ~/.config/zsh/ENV_VARS.sh

# Functions and aliases
source ~/.config/zsh/ALIASES.sh

# Syntax highlighting
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# define highlight colour for commmands
# https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md
# not needed if green okay
# typeset -A ZSH_HIGHLIGHT_STYLES
# ZSH_HIGHLIGHT_STYLES[command]='fg=green'

# Basic auto/tab complete
autoload -U compinit
compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
_comp_options+=(globdots) # Include hidden files

# history-aware autocomplete
autoload -U history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
#bindkey "^[[A" history-beginning-search-backward-end
#bindkey "^[[B" history-beginning-search-forward-end
source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
source ~/.config/zsh/zsh-history-substring-search/zsh-history-substring-search.zsh
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_FOUND="fg=blue"
HISTORY_SUBSTRING_SEARCH_HIGHLIGHT_NOT_FOUND="fg=magenta"
#ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=black"
bindkey '^n' autosuggest-accept

# case-insensitive autocomplete
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# vim keys
bindkey -v
export KEYTIMEOUT=1
# use vim directional keys in tab complete menu
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'h' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -M menuselect '^?' backward-delete-char
# change cursor shape per vim mode
function zle-keymap-select {
    if [[ ${KEYMAP} == vicmd ]] ||
       [[ $1 = 'block' ]]; then
      echo -en '\e[1 q'
    elif [[ ${KEYMAP} == main ]] ||
         [[ ${KEYMAP} == viins ]] ||
         [[ ${KEYMAP} == '' ]] ||
         [[ $1 = 'beam' ]]; then
      echo -ne '\e[5 q'
    fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # use beam cursor on startup
preexec() { echo -ne '\e[5 q' ;} # use beam cursor for each new prompt

# cd to given dir if cd not used
setopt auto_cd
# show completion menu on successive tab press
setopt automenu

# auto startx after login on TTY 1
# if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
#     startx ~/.xinitrc
# fi

# fuzzy finding ('tis magic)
source ~/.config/zsh/fuzzy.zsh

# emit escape sequence to spawn new terminals in current working directory
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

# globbing stuff for negations and idk
setopt extendedglob
