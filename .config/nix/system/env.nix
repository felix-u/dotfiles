{ pkgs, ... }:

let
  theme = import ./theme.nix;
in
{
  environment.sessionVariables = rec {

    FONT_MONO = theme.fontmono;
    FONT_SANS = theme.fontsans;
    FONT_SERIF = theme.fontserif;

    CLR_FG = theme.cfg;
    CLR_BG = theme.cbg;
    CLR_00 = theme.c00;
    CLR_08 = theme.c08;
    CLR_01 = theme.c01;
    CLR_09 = theme.c09;
    CLR_02 = theme.c02;
    CLR_10 = theme.c10;
    CLR_03 = theme.c03;
    CLR_11 = theme.c11;
    CLR_04 = theme.c04;
    CLR_12 = theme.c12;
    CLR_05 = theme.c05;
    CLR_13 = theme.c13;
    CLR_06 = theme.c06;
    CLR_14 = theme.c14;
    CLR_07 = theme.c07;
    CLR_15 = theme.c15;

    CLR_FG_ALT = theme.afg;
    CLR_BG_ALT = theme.abg;
    CLR_00_ALT = theme.a00;
    CLR_08_ALT = theme.a08;
    CLR_15_ALT = theme.a15;

    AGENDAFILE = "$HOME/uni/2023/autumn/agenda";
    AMD_VULKAN_ICD = "RADV";
    BROWSER = "firefox";
    CC = "zig cc";
    DOTFILES = "$HOME/dotfiles";
    DOTSCONF = "$DOTFILES/.config";
    EDITOR = "nvim";
    FILES = "io.elementary.files --new-window";
    GREP_COLORS = "ms=1;97";
    IMAGE = "imv";
    MANWIDTH = "80";
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    PAGER = "less";
    PATH = "$HOME/.local/bin/:$PATH";
    PKG_CONFIG_PATH = "/usr/include";
    PROMPTCHAR = "%";
    PS1 = "$PROMPTCHAR ";
    QT_QPA_PLATFORM = "wayland-egl";
    READER = "zathura";
    RUSTC_WRAPPER = "sccache";
    SDL_VIDEODRIVER = "wayland";
    TERMINAL = "foot";
    TERM_ITALICS = "true";
    VIDEO = "mpv";
    VISUAL = "nvim";
    XCURSOR_SIZE = "24";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CURRENT_DESKTOP = "sway";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  environment.shellAliases = {
    agnota = "nota --no-colour --line-num";
    ag = "agnota $AGENDAFILE -un task --no-colour | $PAGER";
    agerrands = "agnota $AGENDAFILE -un errand | $PAGER";
    agerrandspast = "agnota $AGENDAFILE -bs ascending -n Errand --not-tagged | $PAGER";
    agpast = "agnota $AGENDAFILE -bs ascending -n task --not-tagged | $PAGER";
    agtodo = "agerrandspast && agerrands && agpast && ag";
    agv = "$EDITOR $AGENDAFILE";

    aggpreset = "agg --theme solarized-dark --font-family 'JetBrains Mono'  --font-size 25";

    cdu = "cd ~/uni/2023/autumn";

    gdb = "gdb -tui -ex 'set style enabled off'";

    gitcom = "git add . && git commit -a && git push";

    htop = "htop --no-colour";

    shutdwn = "shutdown -h now";

    timetable = "agnota $AGENDAFILE -n Timetable | $PAGER";

    less = "less -FIRX";

    ls = ''
      /usr/bin/env ls \
        --almost-all - -classify - -color=never --group-directories-first -1'';

    make = "make -j4";

    mkdir = "mkdir -pv";

    mv = "mv -i";
    cp = "cp -i";
    ln = "ln -i";

    nb = "nix-build '<nixpkgs>' -A";

    newsraft = "newsraft -d $XDG_CONFIG_HOME/newsraft/newsraft.sqlite3";

    ncdu = "ncdu --color off";

    onefetch = "onefetch --true-color never";

    pager = "$PAGER";

    prompts = "$EDITOR ~/uni/misc/prompts";

    rainsh = "~/dotfiles/scripts/rain.sh";

    rebuild = "doas $XDG_CONFIG_HOME/system/sysbuild";

    river = "dbus-run-session -- river";

    schemereload = "~/dotfiles/scripts/schemereload.sh";

    swaptheme = "~/dotfiles/scripts/theme/swaptheme.sh";

    sway = "dbus-run-session -- sway";

    v = "nvim";

    weather = "curl 'wttr.in/dc?m&format=3'";
    weatherreport = "curl 'wttr.in/dc?m&format=v2d' | $PAGER";

    wfrec = ''
      mkdir -p ~/Desktop/recordings; \
      wf-recorder -f ~/Desktop/recordings/"$(date +%Y-%m-%d-%H%M)".mp4'';
  };

  environment.systemPackages =
    let
      script = pkgs.writeShellScriptBin;
    in
    [
      (script "clrpick" ''
        #!/usr/bin/env sh
        while true; do
            position=$(slurp -b 00000000 -p)
            sleep 0.2
            if command -v /usr/bin/gm &> /dev/null; then
                color=$(grim -g "$position" -t png - \
                    | /usr/bin/gm convert - -format '%[pixel:p{0,0}]' txt:- \
                    | tail -n 1 \
                    | rev \
                    | cut -d ' ' -f 1 \
                    | rev
                )
            else
                color=$(grim -g "$position" -t png - \
                    | convert - -format '%[pixel:p{0,0}]' txt:- \
                    | tail -n 1 \
                    | cut -d ' ' -f 4
                )
            fi

            echo $color | pastel format hex
            echo $color | wl-copy -n
            sleep 1
        done
      '')

      (script "fman" ''
        #!/usr/bin/env sh
        PAGES=$(man -k . | cut -d ' ' -f 1-2 | tr -d '(' | tr -d ')')
        SELECT=$(echo "$PAGES" | fzf --preview "whatis {1} -s {2} | head -n 1" --preview-window=80%)
        [ $? != 0 ] && return 1
        SECTION=$(echo "$SELECT" | awk '{print $1}')
        CMD=$(echo "$SELECT" | awk '{print $2}')
        man "$SECTION" "$CMD"
      '')

      (script "fetch" ''
        #!/usr/bin/env sh
        bold="$(tput bold)"
        reset="$(tput sgr0)"
        spacer="    "

        printf "$bold"
        printf "OS"
        printf "$reset$spacer"
        uname -mrs
    
        printf "$bold"
        printf "WM"
        printf "$reset$spacer"
        echo "river"
    
        printf "$bold"
        printf "TE"
        printf "$reset$spacer"
        echo "$TERMINAL"

        printf "$bold"
        printf "SH"
        printf "$reset$spacer"
        echo "$(basename $SHELL)"

        printf "\n"

        printf "$bold"
        printf "MEM"
        echo "$reset"
        memnum="$(cat /proc/meminfo | grep MemTotal | sed 's/^[^0-9]*//g' | cut -d ' ' -f 1)"
        gbnum="$(echo "$memnum / 1000000" | bc)"
        echo "$gbnum GB"
        printf "\n"

        printf "$bold"
        printf "CPU"
        echo "$reset"
        cpuinfo="$(cat /proc/cpuinfo)"
        echo "$cpuinfo" | grep -m 1 "model name" | sed 's/\t/ /g'
        echo "$cpuinfo" | grep -m 1 "cores" | sed 's/\t/ /g'
        printf "\n"

        printf "$bold"
        printf "GPU"
        echo "$reset"
        glxinfo | grep -m 1 "Device" | sed 's/^[ ]*//g; s/(.*//g'
      '')

      (script "fontlook" ''
        #!/usr/bin/env sh
        preview_text="ABCDEFGHIJKLM
        NOPQRSTUVWXYZ
        abcdefghijklm
        nopqrstuvwxyz
        1234567890
        !@$\%(){}[]"
        tmpfile="/tmp/fontlook.png"
        while true; do
            font="$(convert -list font | awk -F: '/^[ ]*Font: /{print substr($NF,2)}' | fzf)"
            [ "$font" = "" ] && break
            convert -size 4000x3000 xc:"#$CLR_BG" -fill "#$CLR_FG" \
                -gravity center -pointsize 250 -font "$font" -annotate +0+0 \
                "$preview_text" -flatten "$tmpfile"
            imv "$tmpfile"
        done
        rm "$tmpfile"
      '')

      (script "guide" ''
        #!/usr/bin/env sh
        if [ $# -eq 0 ]; then
            agnota ~/uni/misc/guide.md | $PAGER
        else
            agnota ~/uni/misc/guide.md -n "$@" | $PAGER
        fi
      '')

      (script "gitall" ''
        #!/usr/bin/env sh
        if [ "$1" = "pull" ]; then
            CWD="$(pwd)"
            echo "dotfiles" && cd ~/dotfiles/ && git pull
            echo "uni" && cd ~/uni && git pull
            echo "privateconfig" && cd ~/privateconfig && git pull
            cd "$CWD" || exit
        elif [ "$1" = "com" ]; then
            CWD="$(pwd)"
            echo "dotfiles" && cd ~/dotfiles && gitcom
            echo "uni" && cd ~/uni && gitcom
            echo "privateconfig" && cd ~/privateconfig && gitcom
            cd "$CWD" || exit
        else
            echo "Requires option \"pull\" or \"com\""
            return 1
        fi
      '')

      (script "lc" ''
        #!/usr/bin/env sh
        ls "$@" | 9 mc -N80
      '')

      (script "la" ''
        #!/usr/bin/env sh
        ls "$@" \
            --human-readable \
            --no-group \
            --time-style=long-iso \
            -o
      '')

      (script "mdread" ''
        #!/usr/bin/env sh
        pandoc "$1" --to html5 | w3m -T text/html
      '')

      (script "mkcd" ''
        #!/usr/bin/env sh
        mkdir -p "$1"
        cd "$1" || exit
      '')

      (script "nrs" ''
        #!/usr/bin/env sh
        if [ "$(hostname)" = "thonkpad" ]; then
            doas nixos-rebuild switch --fast \
                -I nixos-config="$XDG_CONFIG_HOME"/nix/thinkpad/configuration.nix "$@"
        elif [ "$(hostname)" = "nixbtw" ] || [ "$(hostname)" = "pc" ]; then
            doas nixos-rebuild switch --fast \
                -I nixos-config="$XDG_CONFIG_HOME"/nix/pc/configuration.nix "$@"
        elif [ "$(hostname)" = "toshiba" ]; then
            doas nixos-rebuild switch --fast \
                -I nixos-config="$XDG_CONFIG_HOME"/nix/toshiba/configuration.nix "$@"
        else
            echo "No config corresponding to this machine's hostname"
            return 1
        fi
      '')

      (script "ready" ''
        #!/usr/bin/env sh
        echo
        echo -e " \e[41m            \e[0m"
        echo -e " \e[43m          \e[0m"
        echo -e " \e[46m        \e[0m"
        echo -en " \e[44m      \e[0m    "; date +%a
        echo -en " \e[45m    \e[0m    "; date +%H:%M
        echo
        echo " READY."
        echo
      '')

      (script "removedupframes" ''
        #!/usr/bin/env sh
        [ $# -lt 2 ] && echo "Not enough arguments supplied" && exit
        [ -z "$3" ] && PRESET="medium"
        [ -n "$3" ] && PRESET="$3"
        ffmpeg -y -i "$1" -vf mpdecimate,setpts=N/FRAME_RATE/TB \
               -an -preset "$PRESET" "$2"
      '')

      (script "resize4k" ''
        #!/usr/bin/env sh
        convert "$1" -resize 4000 "$1"
        echo "Resized $1"
      '')

      (script "rg" ''
        #!/usr/bin/env sh
        /usr/bin/env rg \
            --colors 'match:none' --colors 'match:style:bold' \
            --colors 'match:bg:black' --colors 'path:none' \
            --colors 'path:style:underline' \
            --column --no-heading --smart-case \
            $@
      '')
    ];
}

