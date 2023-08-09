#!/usr/bin/env bash

# BOLD="\e[1;1m"
# CYAN="\e[1;36m"
# RESET="\e[0m"
# PS1="${BOLD}${CYAN}\w\n${RESET}${BOLD}${PROMPTCHAR}${RESET} "

# set -o vi

if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi
