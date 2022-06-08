alias shutdwn="shutdown -h now"
alias weather="curl 'wttr.in/dc?m&format=3'"
alias weatherreport="curl 'wttr.in/dc?m&format=v2d'"

alias cat="bat --style plain"
alias clock="tty-clock -c -C 6 -D"
alias cmatrix="cmatrix -u 2"
alias dsk="startx \"$HOME/.xinitrc\""
alias cdu="cd ~/uni/2022/spring"

# fd (find) - what is it with people hardcoding colours instead of using ansi??
alias fd="fd --color never -uu"

# my fontpreview version
# sk instead of fzf, xres colours, and imv rather than sxiv
alias fontpreview="~/dotfiles/scripts/fontpreview"

alias h="hx"

alias la="ls --group-directories-first --color=always -lAh"
alias ls="ls --group-directories-first --color=always -Ah"

alias ytfzf="ytfzf -t --thumb-viewer=catimg --thumbnail-quality=medium"
alias vim="nvim"
alias v="hx"
alias onefetch="onefetch --true-color never"

# mkdir create parents
alias mkdir='mkdir -pv'

# confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias rmt="trash"

alias ksp='progl /mnt/sda1/Games/KSP_linux/KSP.x86_64'

alias pipes="pipes-rs"

# bash rain
alias rainsh="~/dotfiles/scripts/rain.sh"

# ripgrep - ignore .gitignore and show hidden files, like grep
alias rg="rg -uuu"

# fuzzy finding ftw
alias sk="sk --color=16 --reverse"

# get temps by watching lm_sensors every half a second
alias temps="watch -n 0.5 sensors"

alias top="btm -b"

# build package from repos locally
alias nb="nix-build '<nixpkgs>' -A"

# doasedit
alias doasedit="doas $EDITOR $1"

# TODO: fix truecolour
# use emacs as terminal when launched from the terminal
alias e="TERM=tmux emacsclient -c -nw"
