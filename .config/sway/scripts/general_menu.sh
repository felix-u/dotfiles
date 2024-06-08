#!/usr/bin/env sh

PADDINGV="4%"
PADDINGH="2%"

tofi --font "$FONT_SANS" --width 36% --hint-font false \
    --background-color "#$CLR_BG" --text-color "#$CLR_15" \
    --font-size 20 --prompt-text " $1% " --outline-width 0 --border-width 4 \
    --border-color "#$CLR_FG" --selection-color "$CLR_BG" \
    --selection-background "#$CLR_FG" --selection-background-padding 4 \
    --result-spacing 16 --padding-top $PADDINGV --padding-bottom $PADDINGV \
    --padding-left $PADDINGH --padding-right $PADDINGH --fuzzy-match true
