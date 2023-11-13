#!/usr/bin/env bash

BOLD="\[\e[1;1m\]"
CYAN="\[\e[1;36m\]"
RED="\[\e[1;31m\]"
RESET="\[\e[0m\]"
PS1="${BOLD}${CYAN}\w\n${RESET}${BOLD}\$(if [[ \$? != 0 ]]; then echo -e \"${RED}\"; fi)${PROMPTCHAR}${RESET} "

# set -o vi

if [ -e "$HOME"/.nix_profile/etc/profile.d/nix.sh ]; then
    "$HOME"/.nix_profile/etc/profile.d/nix.sh
fi

# see  https://codeberg.org/dnkl/foot/wiki#user-content-spawning-new-terminal-instances-in-the-current-working-directory

osc7_cwd() {
    local strlen=${#PWD}
    local encoded=""
    local pos c o
    for (( pos=0; pos<strlen; pos++ )); do
        c=${PWD:$pos:1}
        case "$c" in
            [-/:_.!\'\(\)~[:alnum:]] ) o="${c}" ;;
            * ) printf -v o '%%%02X' "'${c}" ;;
        esac
        encoded+="${o}"
    done
    printf '\e]7;file://%s%s\e\\' "${HOSTNAME}" "${encoded}"
}
PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }osc7_cwd
