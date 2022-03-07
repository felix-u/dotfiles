# function to return xresources colours (e.g. #808080) and dpi, etc.
xquery () {
    xrdb -query | grep $1 | awk '{print $NF; exit}'
}

bspc config focused_border_color "$(xquery color4)"
bspc config normal_border_color "$(xquery color0)"
bspc config active_border_color "$(xquery color8)"

bspc config presel_feedback_color "$(xquery color8)"
# bspc config normal_locked_border_color "$(xquery color2)"
# bspc config focused_locked_border_color "$(xquery color2)"
# bspc config normal_sticky_border_color "$(xquery color3)"
# bspc config focused_sticky_border_color "$(xquery color3)"
# bspc config normal_private_border_color "$(xquery color0)"
# bspc config focused_private_border_color "$(xquery color0)"
