#!/bin/bash

# see if muted
if [ $(pacmd list-sinks | awk '/muted/ { print $2 }' | tail -n 1) == "yes" ]
then # muted
    echo 
else
    ALVL=$(./audio.sh)
    # different icon based on volume
    if [ ${ALVL:0:2} -lt 30 ]; then
        echo 
    elif [ ${ALVL:0:2} -lt 60 ]; then
        echo 
    else
        echo 
    fi
fi
