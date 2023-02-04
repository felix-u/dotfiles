#!/usr/bin/env bash

# history
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

# Environment variables
source "$XDG_CONFIG_HOME"/sh/env.sh

# Functions and aliases
source "$XDG_CONFIG_HOME"/sh/aliases_funcs.sh

# Starship prompt
eval "$(starship init bash)"
