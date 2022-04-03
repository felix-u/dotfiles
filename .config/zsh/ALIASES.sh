mkcd ()
{
	mkdir -p -- "$1" &&
	cd -P -- "$1"
}
alias shutdwn="shutdown -h now"
alias weather="curl 'wttr.in/dc?m&format=3'"
alias weather-report="curl 'wttr.in/dc?m&format=v2d'"
gitcom () {
    if [ -z "$1" ]
    then
        git add . && git commit -a && git push
    else
        git add . && git commit -a -m "$1 " && git push
    fi
}
stopwatch() {
    date1=`date +%s`;
    while true; do
    days=$(( $(($(date +%s) - date1)) / 86400 ))
    echo -ne "$days day(s) and $(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r";
    sleep 0.1
    done
}
alias pdftojpeg="pdftoppm -jpeg -r 300"
alias vconf="nvim ~/.config/nvim/init.vim"
alias clock="tty-clock -c -C 6 -D"
alias dsk="startx \"$HOME/.xinitrc\""

alias cdu="cd ~/uni/2022/spring"
alias agenda="nvim ~/uni/2022/spring/agenda/agenda.norg"

alias la="ls --group-directories-first --color=always -lAh"
alias ls="ls --group-directories-first --color=always -A"

alias ytfzf="ytfzf -t --thumb-viewer=catimg --thumbnail-quality=medium"
alias vim="nvim"
alias v="nvim"
alias onefetch="onefetch --true-color never"
alias update-mirrors="sudo reflector --latest 10 --protocol https --sort rate --save /etc/pacman.d/mirrorlist"
wf-record () {
    wf-recorder -f ~/Desktop/recordings/$(date +%Y-%m-%d-%H%M).mp4 -e -t \
        -c h264_vaapi -d /dev/dri/renderD128
}
alias cmatrix="unimatrix -s 97"

# mkdir create parents
alias mkdir='mkdir -pv'

# confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias rmt="trash"

resize4k () {
    convert $1 -resize 4000 $1
    echo "Done resizing $1"
}
cropto4k () {
    convert $1 -resize 3840 $2
    convert $2 -extent 3840x2160 -gravity center $2
}

# function to return xresources colours (e.g. #808080) and dpi, etc.
xquery () {
    xrdb -query | grep $1 | awk '{print $NF; exit}'
}

# return hex without hash - useful in some instances
xquerystrip () {
    xquery $1 | tr -d \#
}

tint () {
    convert $3 -fill $(wq $1) -colorize $2 $4
}

alias mclr="~/Desktop/mclr/mclr"

schemereload () {
    xrdb -merge ~/.Xresources
    ~/dotfiles/scripts/schemereload.sh
}

dpi () {
    VAL=$(bc <<< "scale=2; $1 * $(bc <<< "scale=2; $(xquery dpi)/96")");
    printf "%.0f\n" "$(bc <<< "scale=2; $VAL + 0.01")"
}

compress-pdf () {
    gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/$1 \
    -dNOPAUSE -dQUIET -dBATCH -sOutputFile=$3 $2 # possible settings are screen, ebook, prepress, printer, and default
}

alias ksp='progl /mnt/sda1/Games/KSP_linux/KSP.x86_64'

genclrlist () {
    touch ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq foreground)" --number $1 | pastel format hex > ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color0)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color1)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color2)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color3)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color4)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color5)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color6)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color7)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq background)" "$(wq color8)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color9)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color10)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color11)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color12)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color13)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color14)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
    pastel gradient "$(wq foreground)" "$(wq color15)" --number $1 | pastel format hex >> ~/.config/zsh/clrlist
}

colourise () {
    ImageColorizer $1 $1 -p $(cat ~/.config/zsh/clrlist)
}

clrsimple () {
    ImageColorizer $1 $1 -p $(wq backround) $(wq foreground) \
        $(wq color1) $(wq color2) $(wq color3) $(wq color4) \
        $(wq color5) $(wq color6) $(wq color7) $(wq color8) \
        $(wq color9) $(wq color10) $(wq color11) \
        $(wq color12) $(wq color13) $(wq color14) \
        $(wq color15)
}

# fuzzy find all the things
alias sk="sk --color=16 --reverse"
alias fd='cd $(exa -a1D | sk --preview="exa -a1 {}")'
alias fv='nvim $(sk --preview="bat --color=always -p {}")'
# pacman
alias fpi="paru -Slq | sk --multi --preview 'pacman -Si {1}' | xargs -ro paru -S"
alias fpq="paru -Slq | sk --multi --preview 'pacman -Qi {1}' | xargs -ro echo"
alias fpr="paru -Qq | sk --multi --preview 'pacman -Qi {1}' | xargs -ro paru -Rns"

# file manager (nnn)
if [ -f /usr/share/nnn/quitcd/quitcd.bash_zsh ]; then
    source /usr/share/nnn/quitcd/quitcd.bash_zsh
fi

# colour picker on wayland
clrpick () {
    while true; do
        grim -g "$(slurp -b "00000000" -p)" - -t png -o | \
            convert png:- -format '%[pixel:s]\n' info:- | \
            awk -F '[(,)]' '{printf("#%02x%02x%02x\n",$2,$3,$4)}' | \
            pastel format hex
        sleep 0.5
    done
}

# bash rain
alias rainsh="~/dotfiles/scripts/rain.sh"

# get temps by watching lm_sensors every half a second
alias temps="watch -n 0.5 sensors"

# use custom ASCII with neofetch
# alias neofetch="neofetch --ascii_colors 1 2 3 4 5 6 \
#     --ascii ~/dotfiles/misc/archascii"

# my fontpreview version
# sk instead of fzf, xres colours, and imv rather than sxiv
alias fontpreview="~/dotfiles/scripts/fontpreview"

# dmenu-wl options
source "$XDG_CONFIG_HOME"/zsh/dmenu-aliases.sh

# pomodoro timer
alias pomo="~/dotfiles/scripts/pomo.sh"

# read markdown with w3m
mdread () {
    pandoc $1 --to html5 | w3m -T text/html
}

# pull or commit all changes to personal repos
gitall () {
    if [ $1 = "pull" ]; then
        CWD=$(pwd)
        echo "dotfiles" && cd ~/dotfiles && git pull
        echo "uni" && cd ~/uni && git pull
        echo "privateconfig" && cd ~/privateconfig && git pull
        cd $CWD
    elif [ $1 = "com" ]; then
        CWD=$(pwd)
        echo "dotfiles" && cd ~/dotfiles && gitcom
        echo "uni" && cd ~/uni && gitcom
        echo "privateconfig" && cd ~/privateconfig && gitcom
        cd $CWD
    else
        echo "Requires option \"pull\" or \"com\""
    fi
}

# swap between light and dark terminal theme
themeterm () {
    if [ $1 = 'l' ]; then
        theme.sh < ~/dotfiles/scripts/theme/lighttheme
    elif [ $1 = 'd' ]; then
        theme.sh < ~/dotfiles/scripts/theme/darktheme
    else
        echo "Requires option \"d\" or \"l\""
    fi
}
