#!/usr/bin/env sh

# save current directory
CWD="$(pwd)"
# get directory of script
SCRIPTDIR="$(dirname "$(readlink -f "$0")")"

# option to build
if [ "$1" = "build" ]; then
    # save absolute path to private build file
    BUILDFILE="$(realpath ./private-build-plans.toml)"

    # pull official iosevka
    cd || exit
    mkdir -p git && cd git || exit
    { git clone --depth 1 \
        "https://github.com/be5invis/Iosevka" ./iosevka-custom \
        && cd iosevka-custom; } || { cd iosevka-custom || exit && git pull; }

    # copy private build file to iosevka repo
    if [ -f "./dist/iosevka-custom/ttf/iosevka-custom-medium.ttf" ]; then
        echo "Built already. Delete dist and rerun."
    else
        cp "$BUILDFILE" ./
        # build
        npm install || exit
        npm run build -- contents::iosevka-custom || exit
    fi

    # copy dist folder and iosevka licence to dots
    { cp -r ./dist "$SCRIPTDIR" && echo " Copied builds to dotfiles"; } || exit
    cp ./LICENSE.md "$SCRIPTDIR/Iosevka_LICENCE.md"

# patch with nerdfont glyphs
elif [ "$1" = "patch" ]; then
    if [ -d "$SCRIPTDIR/dist" ]; then
        # pull nerd fonts for the font patcher
        cd || exit
        mkdir -p git && cd git || exit
        { git clone --depth 1 \
            "https://github.com/ryanoasis/nerd-fonts" ./nerd-fonts \
            && cd nerd-fonts; } || { cd nerd-fonts || exit && git pull; }

        # do the patching itself
        for fontfile in "$SCRIPTDIR"/dist/iosevka-custom/ttf/*; do
            ./font-patcher --fontawesome --fontawesomeextension --fontlinux \
                --octicons --powersymbols --pomicons --powerline \
                --powerlineextra --mdi --weather --careful \
                "$fontfile"
        done

        cp ./license-audit.md "$SCRIPTDIR/NerdFonts_LICENCES.md"

        for everything in "$SCRIPTDIR"/dist/*; do
            rm -rf "$everything"
        done
        for patchedfont in ~/git/nerd-fonts/*.ttf*; do
            cp "$patchedfont" "$SCRIPTDIR/dist/"
        done

    else
        echo "No dist folder found. Build first."
        exit 1
    fi

elif [ "$1" = "installpatch" ]; then
    if [ -d "$SCRIPTDIR/dist" ]; then
        echo "Root needed to (re)install fonts to /usr/share/fonts/TTF"
        for oldfont in /usr/share/fonts/TTF/iosevka-custom*.ttf; do
            sudo rm "$oldfont"
        done
        for fontfile in "$SCRIPTDIR"/dist/*.ttf*; do
            sudo cp "$fontfile" /usr/share/fonts/TTF
        done
        echo "Installed to /usr/share/fonts/TTF"
    else
        echo "No patched fonts found. Build and patch first."
        exit 1
    fi

# options to put built font files in appropriate directories
elif [ "$1" = "installvanilla" ]; then
    if [ -d "$SCRIPTDIR/dist" ]; then
        echo "Root needed to install fonts to /usr/share/fonts/TTF"
        sudo \
            cp -r "$SCRIPTDIR/dist/iosevka-custom/ttf/." /usr/share/fonts/TTF \
            && echo "Installed to /usr/share/fonts/TTF"
    else
        echo "No dist folder found. Build first."
        exit 1
    fi

# fail otherwise
else
   echo "Missing argument: \"build\", \"patch\", \"installvanilla\", or \
       \"installpatch\""
   exit 1
fi

cd "$CWD" || exit
