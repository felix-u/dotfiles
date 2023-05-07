#!/usr/bin/env sh

wq () {
    grep "$1:" ~/.Xresources | awk '{print $2}'
}

PADDINGV="4%"
PADDINGH="2%"

tofi --font "$(wq fontmono)" --width 36% --height 50% --hint-font false \
     --background-color "$(wq background)" --text-color "$(wq color15)" \
     --font-size 20 --prompt-text " λ " --outline-width 0 --border-width 4 \
     --border-color "$(wq color8)" --selection-color "$(wq background)" \
     --selection-background "$(wq foreground)" --selection-padding 16 \
     --result-spacing 9 --padding-top $PADDINGV --padding-bottom $PADDINGV \
     --padding-left $PADDINGH --padding-right $PADDINGH --fuzzy-match true