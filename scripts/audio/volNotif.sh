VOL=$(pulsemixer --get-volume | cut -d " " -f1)
dunstify "Volume: " -h int:value:$VOL
