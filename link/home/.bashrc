#!/usr/bin/env bash

export XDG_CONFIG_HOME="$HOME"/.config
source "$XDG_CONFIG_HOME"/sh/environment_variables.sh
source "$XDG_CONFIG_HOME"/sh/aliases_and_functions.sh

BOLD="\[\e[1;1m\]"
CYAN="\[\e[1;36m\]"
RED="\[\e[1;31m\]"
RESET="\[\e[0m\]"
PS1="${BOLD}${CYAN}\w\n${RESET}${BOLD}\$(if [[ \$? != 0 ]]; then echo -e \"${RED}\"; fi)%${RESET} "

mkcd () {
    mkdir -p "$1" && cd "$1"
}
