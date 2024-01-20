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
    PAGER = "less";
    PATH = "$HOME/.local/bin/:$PATH";
    PKG_CONFIG_PATH = "/usr/include";
    PROMPTCHAR = "%";
    PS1 = "$PROMPTCHAR ";
    READER = "zathura";
    RUSTC_WRAPPER = "sccache";
    TERMINAL = "foot";
    TERM_ITALICS = "true";
    VIDEO = "mpv";
    VISUAL = "nvim";
    XCURSOR_SIZE = "24";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };

  environment.shellAliases = {
    agnota = "nota --line-num";
    ag = "agnota $AGENDAFILE -un task --not-tagged";
    agerrands = "agnota $AGENDAFILE -un errand";
    agerrandspast = "agnota $AGENDAFILE -bs ascending -n Errand --not-tagged";
    agpast = "agnota $AGENDAFILE -bs ascending -n task --not-tagged";
    agtodo = "cat <(agerrandspast) <(agerrands) <(agpast) <(ag) | $PAGER";
    agv = "$EDITOR $AGENDAFILE";

    cdu = "cd ~/uni/2023/autumn";

    gdb = "gdb -tui -ex 'set style enabled off'";

    gitcom = "git add . && git commit -a && git push";

    shutdwn = "shutdown -h now";

    less = "less -FIRX";

    ls = ''
      /usr/bin/env ls \
        --almost-all --classify --color=never --group-directories-first -1'';

    make = "make -j4";

    mkdir = "mkdir -pv";

    mv = "mv -i";
    cp = "cp -i";
    ln = "ln -i";

    nb = "nix-build '<nixpkgs>' -A";

    newsraft = "newsraft -d $XDG_CONFIG_HOME/newsraft/newsraft.sqlite3";

    ncdu = "ncdu --color off";

    pager = "$PAGER";

    river = "dbus-run-session -- river";

    sway = "dbus-run-session -- sway";

    v = "nvim";
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
            echo "dotfiles" && cd ~/dotfiles && \
                git add . && git commit -a && git push
            echo "uni" && cd ~/uni && \
                git add . && git commit -a && git push
            echo "privateconfig" && cd ~/privateconfig && \
                git add . && git commit -a && git push
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
            --almost-all \
            --classify \
            --color=never \
            --group-directories-first \
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
                -I nixos-config="$HOME"/dotfiles/.config/nix/thinkpad/configuration.nix "$@"
        elif [ "$(hostname)" = "nixbtw" ] || [ "$(hostname)" = "pc" ]; then
            doas nixos-rebuild switch --fast \
                -I nixos-config="$HOME"/dotfiles/.config/nix/pc/configuration.nix "$@"
        elif [ "$(hostname)" = "toshiba" ]; then
            doas nixos-rebuild switch --fast \
                -I nixos-config="$HOME"/dotfiles/.config/nix/toshiba/configuration.nix "$@"
        else
            echo "No config corresponding to this machine's hostname"
            return 1
        fi
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
    ];
}

