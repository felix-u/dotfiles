alias aggpreset="agg --theme solarized-dark --font-family 'JetBrains Mono'  --font-size 25"

set -l AGENDAFILE ~/uni/2023/spring/agenda
set -l NOTAFLAGS "--force-colour --line-num"
alias agenda="nota $AGENDAFILE -un Task $NOTAFLAGS | $PAGER"
alias shouldve="nota $AGENDAFILE -bs ascending -n Task --not-tagged $NOTAFLAGS | $PAGER"
alias todo="clear && shouldve && agenda"
alias vagenda="$EDITOR $AGENDAFILE"
alias timetable="nota $AGENDAFILE -n Timetable --force-colour | $PAGER"

alias shutdwn="shutdown -h now"
alias weather="curl 'wttr.in/dc?m&format=3'"
alias weatherreport="curl 'wttr.in/dc?m&format=v2d' | $PAGER "

# if type -q bat
#     alias cat="bat --style plain"
# end
alias clock="tty-clock -c -C 6 -D"
alias cmatrix="cmatrix -u 2"
# alias dsk="startx \"$HOME/.xinitrc\""
# alias dsk="sx \"$HOME/.xinitrc\""
alias cdu="cd ~/uni/2023/spring"

# # fd (find) - what is it with people hardcoding colours instead of using ansi??
# alias fd="fd --color never -uu"

# my fontpreview version
# xres colours and imv rather than sxiv
alias fontpreview="~/dotfiles/scripts/fontpreview"

alias grep="grep --exclude-dir .git"

# if type -q lsd
#     alias la="lsd -lA"
#     alias ls="lsd -A --icon never"
# else
alias la="ls -ogAhF --group-directories-first --color=yes --time-style=long-iso"
alias ls="ls -AF --group-directories-first --color=yes"
# end

alias ytfzf="ytfzf -t --thumb-viewer=catimg --thumbnail-quality=medium"
# alias vim="nvim"
alias v="nvim"
alias onefetch="onefetch --true-color never"

# less should render ANSI colour codes, not page if the content fills one,
# screen, ignore case when searching, and leave the contents on the screen
# after quitting
alias less="less -FIRX"

# mkdir create parents
alias mkdir='mkdir -pv'

# confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
# alias rmt="trash"

alias g='gdb -tui'
alias ksp='progl /mnt/sda1/Games/KSP_linux/KSP.x86_64'

# bash rain
alias rainsh="~/dotfiles/scripts/rain.sh"

# # ripgrep - ignore .gitignore and show hidden files, like grep
# alias rg="rg -uuu"

alias swaptheme="~/dotfiles/scripts/theme/swaptheme.sh"

# get temps by watching lm_sensors every half a second
alias temps="watch -n 0.5 sensors"

# build package from repos locally
alias nb="nix-build '<nixpkgs>' -A"

# doasedit
alias doasedit="doas $EDITOR $1"

# TODO: fix truecolour
# use emacs as terminal when launched from the terminal
alias e="TERM=tmux emacsclient -c -nw"