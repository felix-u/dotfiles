rm -rf makesrcclr makesrctint
for old in cafe-walls/*; do
    rm "$old"
done

# function to return xresources colours (e.g. #808080) and dpi, etc.
xquery () {
    xrdb -query | grep $1 | awk '{print $NF; exit}'
}

tint () {
    convert $3 -fill $(xquery $1) -colorize $2 $4
}

#######################
cp -r srctint makesrctint
for img in makesrctint/*.jpg; do
    tint color0 50 "$img" "$img"
done
#######################
mv makesrctint/* cafe-walls/

colourise () {
    ImageColorizer "$1" "$1" -p $(cat ~/.config/zsh/clrlist)
}

#######################
cp -r srcclr makesrcclr
for file in makesrcclr/*.jpg; do
    colourise "$file"
done
#######################
mv makesrcclr/* cafe-walls/

rmdir makesrctint makesrcclr
