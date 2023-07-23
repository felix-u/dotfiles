#!/usr/bin/env sh

PADDINGV="4%"
PADDINGH="2%"
tofi-drun --font "$FONT_SANS" --width 36% --height 50% --hint-font false \
    --background-color "#$CLR_BG" --text-color "#$CLR_15" \
    --font-size 20 --prompt-text " % " --outline-width 0 --border-width 4 \
    --border-color "#$CLR_FG" --selection-color "#$CLR_BG" \
    --selection-background "#$CLR_FG" --selection-background-padding 4 \
    --result-spacing 16 --padding-top $PADDINGV --padding-bottom $PADDINGV \
    --padding-left $PADDINGH --padding-right $PADDINGH --fuzzy-match true \
    --drun-launch true

# dmenu-wl_run -i -h 36 -fn "$(wq fontsans) Medium 12" -nb "$(wq background)" \
#     -nf "$(wq color15)" -sb "$(wq color0)" -sf "$(wq color4)"

# # or FUZZEL
# fuzzel -f 'Fira Sans' -b '${WSBG}ff' -t '${WS07}ff' \
#     -s '${WS00}ff' -S '${WSFG}ff' -m '${WS04}ff' -B 2 \
#     -r 13 -C '${WS00}ff' -x 45 -y 30 --line-height=25 -w 25 --lines 9 \
#     -p 5 -i 'Paper'

# # or bemenu
# bemenu-run -H 30 --fn "$(wq fontsans) Medium 12" --tb "$(wq background)" \
#     --tf "$(wq color4)" --fb "$(wq background)" --ff "$(wq color15)" \
#     --nb "$(wq background)" --nf "$(wq color7)" --hb "$(wq color0)" \
#     --hf "$(wq color4)" -i -p "" -n
