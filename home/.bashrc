#!/usr/bin/env bash

# history
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

# set -o vi
# set show-mode-in-prompt on
# set vi-cmd-mode-string "\1\e[2 q\2"
# set vi-ins-mode-string "\1\e[6 q\2"

# Environment variables
source "$XDG_CONFIG_HOME"/sh/zsh/ENV_VARS.sh

# Functions and aliases
source "$XDG_CONFIG_HOME"/sh/zsh/ALIASES.sh

# Starship prompt
eval "$(starship init bash)"
