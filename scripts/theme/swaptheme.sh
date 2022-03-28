#!/usr/bin/env sh

# save backup file
cp ~/.Xresources ~/.Xresources.bckup

# write new file
shgen ~/dotfiles/scripts/theme/genXresourcesSwap ~/.Xresourcestemp
mv ~/.Xresourcestemp ~/.Xresources

# regenerate configs and everything
~/dotfiles/scripts/schemereload.sh

echo "Swapped theme."
