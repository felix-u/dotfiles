#!/usr/bin/env sh

command -v bat > /dev/null && \
    alias cat="bat --style plain"

alias cdu="cd ~/uni/2022/autumn"

command -v tty-clock > /dev/null && \
    alias clock="tty-clock -c -C 6 -D"

command -v cmatrix > /dev/null && \
    alias cmatrix="cmatrix -u 2"

command -v fd > /dev/null && \
    alias fd="fd --color never -uu"

alias fontpreview="~/dotfiles/scripts/fontpreview"

if command -v lsd > /dev/null; then
    alias la="lsd -lA"
    alias ls="lsd -A --icon never"
else
    alias la="ls --group-directories-first --color=always -lAh"
    alias ls="ls --group-directories-first --color=always -Ah"
fi

alias shutdwn="shutdown -h now"

alias weather="curl 'wttr.in/dc?m&format=3'"
alias weatherreport="curl 'wttr.in/dc?m&format=v2d'"
