#!/usr/bin/env sh

clrpick () {
    while true; do
        position=$(slurp -b 00000000 -p)
        sleep 0.2
        if command -v /usr/bin/gm &> /dev/null; then
            color=$(grim -g "$position" -t png - \
                | /usr/bin/gm convert - -format '%[pixel:p{0,0}]' txt:- \
                | tail -n 1 \
                | rev \
                | cut -d ' ' -f 1 \
                | rev
            )
        else
            color=$(grim -g "$position" -t png - \
                | convert - -format '%[pixel:p{0,0}]' txt:- \
                | tail -n 1 \
                | cut -d ' ' -f 4
            )
        fi

        echo $color | pastel format hex
        echo $color | wl-copy -n
        sleep 1
    done
}

fman () {
    PAGES=$(man -k . | cut -d ' ' -f 1-2 | tr -d '(' | tr -d ')')
    SELECT=$(echo "$PAGES" | fzf --preview "whatis {1} -s {2} | head -n 1" --preview-window=80%)
    [ $? != 0 ] && return 1
    SECTION=$(echo "$SELECT" | awk '{print $1}')
    CMD=$(echo "$SELECT" | awk '{print $2}')
    man "$SECTION" "$CMD"
}

fetch () {
    bold="$(tput bold)"
    reset="$(tput sgr0)"
    spacer="    "

    printf "${bold}OS${reset}${spacer}"
    uname -mrs
    
    printf "${bold}WM${reset}${spacer}"
    echo "river"
    
    printf "${bold}TE${reset}${spacer}"
    echo "$TERMINAL"

    printf "${bold}SH${reset}${spacer}"
    echo "$(basename $SHELL)"

    printf "\n"

    echo "${bold}MEM${reset}"
    memnum="$(cat /proc/meminfo | grep MemTotal | sed 's/^[^0-9]*//g' | cut -d ' ' -f 1)"
    gbnum="$(echo "$memnum / 1000000" | bc)"
    echo "$gbnum GB"
    printf "\n"

    echo "${bold}CPU${reset}"
    cpuinfo="$(cat /proc/cpuinfo)"
    echo "$cpuinfo" | grep -m 1 "model name" | sed 's/\t/ /g'
    echo "$cpuinfo" | grep -m 1 "cores" | sed 's/\t/ /g'
    printf "\n"

    echo "${bold}GPU${reset}"
    glxinfo | grep -m 1 "Device" | sed 's/^[ ]*//g; s/(.*//g'
}

guide() {
    if [ $# -eq 0 ]; then
        agnota ~/uni/misc/guide.md | $PAGER
    else
        agnota ~/uni/misc/guide.md -n "$@" | $PAGER
    fi
}


# Pull or commit all changes to personal repos
gitall() {
    if [ "$1" = "pull" ]; then
        CWD="$(pwd)"
        echo "dotfiles" && cd ~/dotfiles/ && git pull
        echo "uni" && cd ~/uni && git pull
        echo "privateconfig" && cd ~/privateconfig && git pull
        cd "$CWD" || exit
    elif [ "$1" = "com" ]; then
        CWD="$(pwd)"
        echo "dotfiles" && cd ~/dotfiles && gitcom
        echo "uni" && cd ~/uni && gitcom
        echo "privateconfig" && cd ~/privateconfig && gitcom
        cd "$CWD" || exit
    else
        echo "Requires option \"pull\" or \"com\""
        return 1
    fi
}

lc () {
    ls "$@" | 9 mc -N80
}

la () {
    ls "$@" \
        --human-readable \
        --no-group \
        --time-style=long-iso \
        -o
}

manpdf () {
    tmpfile="$(mktemp)"
    mv "$tmpfile" "$tmpfile".pdf
    man -Tpdf "$@" > "$tmpfile".pdf
    [ ! "$?" -eq 0 ] && return
    zathura "$tmpfile.pdf"
    rm "$tmpfile".pdf
}

mdread() {
    pandoc "$1" --to html5 | w3m -T text/html
}

mkcd() {
    mkdir -p "$1"
    cd "$1" || exit
}

nrs() {
    if [ "$(hostname)" = "thonkpad" ]; then
        doas nixos-rebuild switch --fast \
	        -I nixos-config="$XDG_CONFIG_HOME"/nix/thinkpad/configuration.nix "$@"
    elif [ "$(hostname)" = "nixbtw" ] || [ "$(hostname)" = "pc" ]; then
        doas nixos-rebuild switch --fast \
            -I nixos-config="$XDG_CONFIG_HOME"/nix/pc/configuration.nix "$@"
    elif [ "$(hostname)" = "toshiba" ]; then
        doas nixos-rebuild switch --fast \
            -I nixos-config="$XDG_CONFIG_HOME"/nix/toshiba/configuration.nix "$@"
    else
        echo "No config corresponding to this machine's hostname"
        return 1
    fi
}

ready () {
    echo
    echo -e " \e[41m            \e[0m"
    echo -e " \e[43m          \e[0m"
    echo -e " \e[46m        \e[0m"
    echo -en " \e[44m      \e[0m    "; date +%a
    echo -en " \e[45m    \e[0m    "; date +%H:%M
    echo "\n READY.\n"
}

resize4k() {
    convert "$1" -resize 4000 "$1"
    echo "Resized $1"
}

rg () {
    /usr/bin/env rg \
        --colors 'match:none' --colors 'match:style:bold' \
        --colors 'match:bg:black' --colors 'path:none' \
        --colors 'path:style:underline' \
        --column --no-heading --smart-case \
        $@
}
