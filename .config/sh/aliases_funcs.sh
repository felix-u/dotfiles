#!/usr/bin/env sh

# agg converts asciinema files to GIFs
alias aggpreset="agg --theme solarized-dark --font-family 'JetBrains Mono'  --font-size 25"

AGENDAFILE="$HOME/uni/2023/spring/agenda"
alias notaflags="nota --force-colour --line-num"
alias agenda="notaflags \$AGENDAFILE -un Task | \$PAGER"
alias agendapast="notaflags \$AGENDAFILE -bs ascending -n Task --not-tagged | \$PAGER"
alias agendav="\$EDITOR \$AGENDAFILE"
alias errands="notaflags \$AGENDAFILE -un Errand | \$PAGER"
alias errandspast="notaflags \$AGENDAFILE -bs ascending -n Errand --not-tagged | \$PAGER"
alias todo="clear && errandspast && errands && agendapast && agenda"
alias timetable="notaflags \$AGENDAFILE -n Timetable | \$PAGER"
guide() {
    if [ $# -eq 0 ]; then
        notaflags ~/uni/misc/guide.md | $PAGER
    else
        notaflags ~/uni/misc/guide.md -n "$@" | $PAGER
    fi
}

alias cdu="cd ~/uni/2023/spring"
alias clock="tty-clock -c -C 6 -D"

clrpick() {
    while true
    do
        grim -g "$(slurp -b "00000000" -p)" - -t png -o | \
            convert png:- -format '%[pixel:s]\n' info:- | \
            awk -F '[(,)]' '{printf("#%02x%02x%02x\n",$2,$3,$4)}' | \
            pastel format hex
        sleep 0.5
    done
}

alias cmatrix="cmatrix -u 2"

alias f="fzf"
FDEPTH=4
fcd () {
    DIR=$(find . -maxdepth "$FDEPTH" -type d -not -path "*/.git/*" \
        -not -path "*cache*" -not -path "*share*" -not -path "*/lib/*" \
        -printf "%P\n" | fzf --preview "tree -shL $FDEPTH --du {}")
    [ $? = 0 ] && cd "$DIR"
}
fdoaj () {
    query="$@"
    [ -z "$query" ] && echo "fdb: expected search keywords" && return 1
    query=$(echo "$query" | sed 's/ /%20/g')
    response=$(curl -s "https://doaj.org/api/v2/search/articles/$query")
    titles=$(printf "%s" "$response" | jq -r '.results[] | .bibjson.title')
    urls=$(printf "%s" "$response" | jq -r '.results[] | .bibjson.link[0].url')
    list=$(paste <(echo "$titles") <(echo "$urls") | column -t -s $'\t')
    echo "$list" | fzf --preview='fdbpreview() {
        url="$1"
        TMP=$(mktemp)
        wget -q --max-redirect=5 "$url" -O "$TMP"
        file_type=$(file -b --mime-type "$TMP")
        if echo "$file_type" | grep -qi "pdf"; then
            PREVTEXT=$(pdftotext "$TMP" - | awk "/abstract/ {flag=1} flag" IGNORECASE=1)
        else
            PREVTEXT=$(w3m -T text/html -dump < "$TMP" | awk "/abstract/ {flag=1} flag" IGNORECASE=1)
        fi
        [ "$PREVTEXT" = "" ] && PREVTEXT="NO PREVIEW"
        rm "$TMP"
        echo "$PREVTEXT"
    }
    fdbpreview {-1}' \
        --preview-window=up:50%:wrap --bind "enter:execute($BROWSER {-1})"
}
# # doesn't work for now, and needs xmlstarlet to be installed
# fdba () {
#     query="$@"
#     [ -z "$query" ] && echo "fdbarxiv: expected search keywords" && return 1
#     query=$(echo "$query" | sed 's/ /+/g')
#     response=$(curl -s "https://export.arxiv.org/api/query?search_query=all:$query&start=0&max_results=100")
#     titles=$(printf "%s" "$response" | xml sel -N x="http://www.w3.org/2005/Atom" -t -m "//x:entry/x:title" -v . -n)
#     urls=$(printf "%s" "$response" | xml sel -N x="http://www.w3.org/2005/Atom" \
#         -t -m "//x:entry/x:id" -v . -n | sed 's/abs/pdf/' | sed 's/$/.pdf/')
#     list=$(paste <(echo "$titles") <(echo "$urls") | column -t -s $'\t')
#     echo "$list" | fzf --preview='fdbpreview() {
#         url=$1
#         PREVTEXT=$(curl "$url" -so - | pdftotext - - | awk "/abstract/ {flag=1} flag" IGNORECASE=1)
#         [ "$PREVTEXT" = "" ] && PREVTEXT="NO PREVIEW"
#         rm "$TMP"
#         echo "$PREVTEXT"
#     }
#     fdbpreview {-1}' \
#         --preview-window=up:50%:wrap --bind "enter:execute($BROWSER {-1})"
# }
femoji () {
    EMOJI=$(curl -sSL 'https://git.io/JXXO7' | tr -d :) # emoji.txt file from a gist
    SELECT=$(echo "$EMOJI" | fzf | awk '{print $1}')
    [ $? = 0 ] && wl-copy "$SELECT"
}
fnota () {
    [ $# -ge 1 ] && FILE="$1" || FILE="$HOME/uni/misc/guide.md"
    if [ ! -e "$FILE" ]; then 
        echo "File does not exist."
        return 1
    fi
    NODES=$(nota --line-num "$FILE" | grep "|" | sed 's/^[ \t]*//' | sort | \
        uniq)
    SELECT=$(echo "$NODES" | fzf --preview \
        "tail --lines=+\$(echo {} | awk -F '|' '{print \$NF}' | \
        awk '{print \$NF}') $FILE")
    [ $? = 0 ] && less +$(echo "$SELECT" | awk -F '|' '{print $NF}' | \
        awk '{print $NF}') "$FILE"
}
fkill () {
    DOOMEDPID=$(ps aux | fzf) 
    [ $? != 0 ] && return 1
    kill -9 $(echo "$DOOMEDPID" | awk '{print $2}')
}
fman () {
    PAGES=$(man -k . | awk '{print $2 " " $1}' | tr -d '()')
    field () {
        NUM=$1
        shift
        echo "$@" | awk "{print \$$NUM}"
    }
    SELECT=$(echo "$PAGES" | \
        fzf --preview "man '$(field 1 {})' '$(field 2 {})'" \
        --preview-window=80%)
    [ $? != 0 ] && return 1
    SECTION=$(echo "$SELECT" | awk '{print $1}')
    CMD=$(echo "$SELECT" | awk '{print $2}')
    man "$SECTION" "$CMD"
}
fsnip () {
    SNIPDIR="$DOTFILES"/misc/snippets
    SNIP=$(/usr/bin/env ls -1 "$SNIPDIR" | \
        fzf --preview "cat $SNIPDIR/{}" --preview-window=80%)
    [ $? != 0 ] && return 1
    wl-copy < "$SNIPDIR"/"$SNIP" && echo "Copied contents of $SNIPDIR/$SNIP to clipboard"
}
fwiki () {
    local api_url="https://en.wikipedia.org/w/api.php"
    local search_term="$@"

    results=$(curl -s -G "${api_url}" \
        --data-urlencode "action=query" \
        --data-urlencode "format=json" \
        --data-urlencode "list=search" \
        --data-urlencode "formatversion=2" \
        --data-urlencode "srsearch=${search_term}" \
        --data-urlencode "srprop=snippet" \
        --data-urlencode "srlimit=50" \
        --data-urlencode "utf8=1")
    titles=$(echo "$results" | jq -r '.query.search[] | .title')

    SELECT=$(echo "$titles" | fzf \
        --preview "curl -s \"${api_url}?action=parse&format=json&page=\$(echo {} \
        | sed 's/ /%20/g')&prop=text\" | jq -r '.parse.text.\"*\"' | \
        w3m -dump -T text/html" --preview-window="70%,wrap")
    [ $? != 0 ] && return 1
    $BROWSER "https://en.wikipedia.org/wiki/$(echo "$SELECT" | sed 's/ /_/g')" & disown
}
fword () {
    WORDS=$(find -L . -maxdepth "$FDEPTH" -type f \
        -not -path "*/.git/*" -not -path "*cache*" -not -path "*share*" \
        -not -path "*/lib/*" -exec grep -Iq . {} \; -printf "%P\n" | \
        xargs grep -Eo '\w+')
    LIST=$(echo "$WORDS" | awk -F : '{print $NF}' | sort -u --ignore-case)
    SELECT=$(echo "$LIST" | fzf --preview "grep --color=always -Ir {} ." \
        --preview-window=80%)
    [ $? != 0 ] && return 1
    find -L . -maxdepth "$FDEPTH" -type f \
        -not -path "*/.git/*" -not -path "*cache*" -not -path "*share*" \
        -not -path "*/lib/*" -exec grep --color=always -Ir "$SELECT" {} +
}

# Runs neofetch with my custom config, which only works on NixOS
alias fetch="printf '\n' && \neofetch"
# Runs neofetch with no config
alias neofetch="neofetch --config none"

# edited to be wayland-compatible
alias fontpreview="~/dotfiles/scripts/fontpreview"

alias g='gdb -tui'

# Pull or commit all changes to personal repos
alias gitcom="git add . && git commit -a && git push"
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

alias grep="grep --exclude-dir .git"

alias htop="htop --no-colour"

imgclrs() {
    imgclr "$@" -p \
    "$(wqs background)" "$(wqs foreground)" "$(wqs color7)" "$(wqs color8)" "$(wqs color15)"
}
imgclrx() {
    imgclr "$@" -p \
    "$(wqs background)" "$(wqs foreground)" "$(wqs color1)" "$(wqs color2)" \
    "$(wqs color3)" "$(wqs color4)" "$(wqs color5)" "$(wqs color6)" \
    "$(wqs color7)" "$(wqs color8)" "$(wqs color9)" "$(wqs color10)" \
    "$(wqs color11)" "$(wqs color12)" "$(wqs color13)" "$(wqs color14)" \
    "$(wqs color15)"
}

alias ksp='progl /mnt/sda1/Games/KSP_linux/KSP.x86_64'

alias la="ls -ogAhF --group-directories-first --time-style=long-iso"
alias ls="ls -AFw 80 --group-directories-first"

# less should render ANSI colour, not page if content fills one screen, ignore
# case when searching, and leave the contents on the screen after quitting.
alias less="less -FIRX"

# Execute jobs in parallel, with a limit of 8.
alias make="make -j8"

mdread() {
    pandoc "$1" --to html5 | w3m -T text/html
}

mkcd() {
    mkdir -p "$1"
    cd "$1" || exit
}

# mkdir create parents
alias mkdir='mkdir -pv'

# confirmations
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'

# build package from repos locally
alias nb="nix-build '<nixpkgs>' -A"

nrs() {
    if [ "$(hostname)" = "thonkpad" ]; then
        doas nixos-rebuild switch --fast \
	        -I nixos-config="$XDG_CONFIG_HOME"/nix/thinkpad/configuration.nix
    elif [ "$(hostname)" = "nixbtw" ]; then
        doas nixos-rebuild switch --fast \
            -I nixos-config="$XDG_CONFIG_HOME"/nix/pc/configuration.nix
    elif [ "$(hostname)" = "toshiba" ]; then
        doas nixos-rebuild switch --fast \
            -I nixos-config="$XDG_CONFIG_HOME"/nix/toshiba/configuration.nix
    else
        echo "No config corresponding to this machine's hostname"
        return 1
    fi
}

# Get diff from latest switch
nvdd() {
    \ls -v /nix/var/nix/profiles/ | tail -n 2 | \
        awk '{print "/nix/var/nix/profiles/" $0}' - | xargs nvd diff
}

# default colour scheme is completely nonsensical
alias ncdu="ncdu --color off"

alias onefetch="onefetch --true-color never"

alias page="\$PAGER"

# ChatGPT prompts
alias prompts="\$EDITOR ~/uni/misc/prompts"

# bash rain
alias rainsh="~/dotfiles/scripts/rain.sh"

resize4k() {
    convert "$1" -resize 4000 "$1"
    echo "Resized $1"
}

alias schemereload="~/dotfiles/scripts/schemereload.sh"

alias shutdwn="shutdown -h now"

alias swaptheme="~/dotfiles/scripts/theme/swaptheme.sh"

swaybgset() {
    pkill swaybg; swaybg -m fill -i "$1"
}

alias tm="tmux"

# get temps by watching lm_sensors every half a second
alias temps="watch -n 0.5 sensors"

alias termatonotif="termato -n \"notify-send '%s'\" -f 25 -b 10 -l 20"

themeterm() {
    if [ "$1" = "l" ]; then
        theme.sh < ~/dotfiles/scripts/theme/lighttheme
    elif [ "$1" = "d" ]; then
        theme.sh < ~/dotfiles/scripts/theme/darktheme
    else
        echo "Requires option \"d\" or \"l\""
        return 1
    fi
}

tint() {
    convert "$3" -fill "$(wq "$1")" -colorize "$2" "$4"
}

alias v="nvim"

alias weather="curl 'wttr.in/dc?m&format=3'"
alias weatherreport="curl 'wttr.in/dc?m&format=v2d' | \$PAGER"

wfrec() {
    mkdir -p ~/Desktop/recordings/
    wf-recorder -f ~/Desktop/recordings/"$(date +%Y-%m-%d-%H%M)".mp4
}
wfrecwindow() {
    mkdir -p ~/Desktop/recordings/
    wf-recorder -f ~/Desktop/recordings/"$(date +%Y-%m-%d-%H%M)".mp4 -g \
	"$(slurp -d -b "$(wq color7)"40 -c "$(wq color7)" -w 3)"
}

alias wm="\$EDITOR ~/uni/workingmemory"

alias ytfzf="ytfzf -t --thumb-viewer=catimg --thumbnail-quality=medium"
