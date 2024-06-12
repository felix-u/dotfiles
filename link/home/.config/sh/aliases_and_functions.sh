#!/usr/bin/env sh

alias gitcom="git add . && git commit -a && git push"
alias la="ls -lA"
alias ls="ls -A"
alias make="make -j4"
alias v="$EDITOR"

gitall() {
    CWD="$(pwd)"
    if [ "$1" = "pull" ]; then
        echo -n "dotfiles: "      && cd "$HOME"/dotfiles      && git pull
        echo -n "uni: "           && cd "$HOME"/uni           && git pull
        echo -n "privateconfig: " && cd "$HOME"/privateconfig && git pull
    elif [ "$1" = "com" ]; then
        echo -n "dotfiles: "      && cd "$HOME"/dotfiles      && git add . && git commit -a && git push
        echo -n "uni: "           && cd "$HOME"/uni           && git add . && git commit -a && git push
        echo -n "privateconfig: " && cd "$HOME"/privateconfig && git add . && git commit -a && git push
    else
        echo "error: expected argument 'pull' or 'com'"
    fi
    cd "$CWD" || exit
}
