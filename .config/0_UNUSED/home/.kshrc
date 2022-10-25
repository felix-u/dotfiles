#!/usr/bin/env oksh
# shellcheck shell=ksh

HISTFILE="$XDG_CACHE_HOME"/.ksh_history
HISTSIZE=20000

set -o emacs

bind "^[[A"=up-history
bind "^[[B"=down-history
bind "^[[C"=forward-char
bind "^[[D"=backward-char

PS1="\[\e[1;36m\]\w\n\[\e[1;34m\]λ \[\e[0m\]"
