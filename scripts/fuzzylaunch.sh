#!/usr/bin/env sh
alias sk="sk --color=16 --reverse --tiebreak=index"

TARGET="$(find /usr/bin/ -type f -executable -printf '%f\n' | sort -d | sk)"
"/usr/bin/$TARGET" &

# kill -9 $PPID
