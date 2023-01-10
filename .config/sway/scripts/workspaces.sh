#!/usr/bin/env sh

OCCUPIED=$(swaymsg -t get_workspaces | jq -r '.[] | .name' | tr '\n' ' ' | head -c -1)
FOCUSED=$(swaymsg -t get_workspaces | jq -r '.[] | select(.focused==true).name' | tr '\n' ' ' | head -c -1)

echo "Occupied: $OCCUPIED"
echo "Focused: $FOCUSED"
