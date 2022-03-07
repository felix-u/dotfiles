#!/bin/sh
# return xresources colours
xquery () {
    xrdb -query | grep $1 | awk '{print $NF; exit}'
}
if [ $(bluetoothctl show | grep "Powered: yes" | wc -c) -eq 0 ]
then
    # OFF
    echo "%{F$(xquery color7)}"
else
  if [ $(echo info | bluetoothctl | grep 'Device' | wc -c) -eq 0 ]
  then
    # ON
    echo "%{F$(xquery color15)}"
  fi
  # Connected
  echo "%{F$(xquery color12)}"
fi

