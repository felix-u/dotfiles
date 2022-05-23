alias shutdwn="shutdown -h now"
alias weather="curl 'wttr.in/dc?m&format=3'"
alias weatherreport="curl 'wttr.in/dc?m&format=v2d'"

alias clock="tty-clock -c -C 6 -D"
alias dsk="startx \"$HOME/.xinitrc\""
alias cdu="cd ~/uni/2022/spring"

alias la="ls --group-directories-first --color=always -lAh"
alias ls="ls --group-directories-first --color=always -A"

alias ytfzf="ytfzf -t --thumb-viewer=catimg --thumbnail-quality=medium"
alias vim="nvim"
alias v="nvim"
alias onefetch="onefetch --true-color never"

# mkdir create parents
alias mkdir='mkdir -pv'

# confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias rmt="trash"

# TODO: remove in favour of nix derivation (maybe)
alias mclr="~/Desktop/mclr/mclr"

alias ksp='progl /mnt/sda1/Games/KSP_linux/KSP.x86_64'

# fuzzy finding ftw
alias sk="sk --color=16 --reverse"

# bash rain
alias rainsh="~/dotfiles/scripts/rain.sh"

# get temps by watching lm_sensors every half a second
alias temps="watch -n 0.5 sensors"

# my fontpreview version
# sk instead of fzf, xres colours, and imv rather than sxiv
alias fontpreview="~/dotfiles/scripts/fontpreview"

# build package from repos locally
alias nb="nix-build '<nixpkgs>' -A"

# doasedit
alias doasedit="doas $EDITOR $1"

# TODO: fix truecolour
# use emacs as terminal when launched from the terminal
alias e="TERM=tmux emacsclient -c -nw"
