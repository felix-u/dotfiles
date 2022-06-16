#!/usr/bin/env sh

# save backup file
cp "$XRESOURCES" ~/.Xresources.bckup

# write new file
shgen ~/dotfiles/scripts/theme/genXresourcesSwap ~/.Xresourcestemp
mv ~/.Xresourcestemp "$XRESOURCES"

# regenerate configs and everything
~/dotfiles/scripts/schemereload.sh

echo "Swapped theme."

rm ~/.Xresources.bckup
