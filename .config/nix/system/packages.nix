{ pkgs, config, lib, ... }:

let
    pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
    hyprland = (import flake-compat {
        src = builtins.fetchTarball "https://github.com/hyprwm/Hyprland/archive/master.tar.gz";
    }).defaultNix;
in {
    imports = [
        hyprland.nixosModules.default
    ];
    nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
            experimental-features = nix-command flakes
        '';
        checkConfig = true;
        gc = {
            automatic = true;
            persistent = true;
            dates = "weekly";
            options = "--delete-older-than 30d";
        };
        optimise.automatic = true;
        settings = {
            auto-optimise-store = true;
            trusted-users = [ "root" "felix" ];
        };
    };
    nixpkgs.config.allowUnfree = true;
    # NUR
    nixpkgs.config.packageOverrides = pkgs: {
        nur = import
            (builtins.fetchTarball
                "https://github.com/nix-community/NUR/archive/master.tar.gz") {
            inherit pkgs;
        };
    };

    # emacs setup and overlay
    services.emacs.package = pkgs.emacsPgtkNativeComp;
    services.emacs.enable = false;
    nixpkgs.overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        ref = "master";
        rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
      }))
        # hyprland.overlays.default
        # (self: super: {
        #     waybar = super.waybar.overrideAttrs (oldAttrs: {
        #         mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
        #     });
        # })
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        }))
    ];

    # programs.hyprland = {
    #     enable = false;
    #     package = pkgs.hyprland;
    # };

    # packages for all systems
    environment.systemPackages =
    let

        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

        # effectively "symlink" sudo to doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        godot4-alpha = import ../derivations/godot4alpha.nix;

        helix-src = builtins.fetchTarball {
            url = "https://github.com/helix-editor/helix/archive/f0f295a6679655dccfab0c1e0e9bb4a87e351db5.tar.gz";
        };
        helix-git = import flake-compat { src = helix-src; };

        # hyprland-src = builtins.fetchTarball {
        #     url = "https://github.com/hyprwm/Hyprland/archive/1626707b7f4fd3d2b313e78cb3a41783f072f73b.tar.gz";
        #     sha256 = "sha256:0lmka2724m0ylsmwd9zkrv8bvhhrn7jvrznn50qqbnp42h27dw11";
        # };
        # hyprland-git = (pkgs.callPackage [ import flake-compat { src = hyprland-src; } {} ]);

        nextvi = (pkgs.callPackage ../derivations/nextvi.nix {});

        odin-dev = pkgs.odin.overrideAttrs (oldAttrs: rec {
            nativeBuildInputs = with pkgs; oldAttrs.nativeBuildInputs ++ [
                which llvmPackages.llvm.dev
            ];
            preBuild = ''
              patchShebangs build_odin.sh
            '';
            src = pkgs.fetchFromGitHub {
                owner = "odin-lang";
                repo = "Odin";
                rev = "dev-2022-09";
                sha256 = "sha256-qBAObLbgry+r/wOsFf7LDWJdOyn7RvEIbFCyAvN0waA=";
            };
            installPhase = ''
                mkdir -p $out/bin
                cp odin $out/bin/odin
                cp -r core $out/bin/core
                CWD="$(pwd)"
                cd vendor/stb/src && ${pkgs.gnumake}/bin/make
                cd "$CWD"
                cp -r vendor $out/bin/vendor
                wrapProgram $out/bin/odin --prefix PATH : ${lib.makeBinPath (with pkgs.llvmPackages; [
                  bintools
                  llvm
                  clang
                  lld
                ])}
            '';
        });

        ols = import ../derivations/ols.nix;

        imgclr = (pkgs.callPackage ../derivations/imgclr.nix {});

        shgen = import ../derivations/shgen.nix;

        themesh = import ../derivations/themesh.nix;

        w4 = import ../derivations/wasm-4.nix;

        zig-master = import ../derivations/zig-master.nix;

    in
    with pkgs; [

        # ESSENTIAL
            foot gh git neofetch nvi pfetch starship stow wget

        # UTILS IN RUST
                # bat # cat
                # fd # find
            helix-git.defaultNix.packages.x86_64-linux.default
                # lsd # ls
                # procs # ps
                # ripgrep # grep
            skim # fzf

        # DEV AND PROGRAMMING
        # misc
            # android-tools bsdiff
            bviplus libresprite
            # pixelorama rx
            # openssl_3_0 pkg-config protobuf unstable.godot godot4-alpha
        # c
            binutils-unwrapped-all-targets clang clang-tools cppcheck
            cmake cmake-language-server gcc gdb gnumake man-pages-posix tinycc
            valgrind
        # go
            go
        # web (HTML, CSS, JS)
            nodejs yarn
            nodePackages.npm nodePackages.js-beautify
        # lisp and emacs
            # clisp emacsPgtkNativeComp emacs-all-the-icons-fonts
            # # clisp sbcl libvterm-neovim
        # lua
            lua sumneko-lua-language-server
        # nix
            any-nix-shell cachix home-manager deadnix nix-index statix rnix-lsp
        # odin
            odin-dev ols
        # python
            python3Full nodePackages.pyright
        # rust
            cargo clippy rust-analyzer
        # shell
            nodePackages.bash-language-server shellcheck
        # vim
            unstable.neovim tree-sitter
            nodePackages.vscode-langservers-extracted
            nodePackages.vim-language-server
        # zig
            zig-master

        # MATHS
            bc gnuplot libqalculate
            # maxima qalculate-gtk wxmaxima

        # TERMINAL MISC
                # dict lowdown
            cava cmatrix doas-as-sudo xdragon entr figlet file ffmpeg fzf
            handlr htop hunspell hunspellDicts.en-gb-ise hyperfine killall
            lm_sensors lolcat ncdu ncspot nvd onefetch
            oneshot pastel pandoc pdftk poppler_utils shgen termdown
            themesh tldr tmux tty-clock udiskie udisks unrar unzip v4l-utils
            libv4l w3m xdg-utils youtube-dl ytfzf zip zsh _7zz
            imgclr # haha, c'est a moi :D

        # INTERNET & BLUETOOTH
            blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

        # DESKTOP
            appimage-run anki-bin bitwarden calibre font-manager gnome.pomodoro handlr
            # hyprland-git
            # hyprland
            # unstable.hyprland
            imagemagick imv libreoffice libnotify
            # libsForQt5.qtstyleplugin-kvantum
            libva libva-utils mpv
            mpvScripts.youtube-quality obs-studio obs-studio-plugins.wlrobs
            pavucontrol
            # profanity
            pulsemixer signal-desktop qt5ct wally-cli
            # xfce.thunar xfce.thunar-archive-plugin
            pcmanfm zathura

        # VISUAL
            gsettings-desktop-schemas gtk-engine-murrine gtk_engines
            solarc-gtk-theme gnome.adwaita-icon-theme
            # numix-solarized-gtk-theme

        # PHOTO, GRAPHICS & VIDEO
            unstable.darktable hugin inkscape-with-extensions jpegoptim krita
            luminanceHDR mediainfo unstable.gimp-with-plugins

        # GAMING
            lutris minecraft unstable.optifine minetest proton-caller
            protontricks protonup unstable.heroic mangohud
            # # retro
            # (retroarchFull.overrideAttrs (oldAttrs: {
            #     cores = oldAttrs.cores ++ [
            #         libretro.tic80
            #     ];
            # }))
            # vice # commodore emulation
            # w4 # wasm-4 fantasy console

        # LATEX
            biber texinfo texlab unstable.texlive.combined.scheme-full

        # KERNEL
            config.boot.kernelPackages.v4l2loopback

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

    # # dict
    # environment.etc."dict.conf".text = ''
    #     server localhost
    # '';
    # environment.etc."conf.d/dictd".text = ''
    #     DICTD_ARGS="--locale en_GB.UTF-8"
    # '';
    # services.dictd = {
    #     enable = false;
    #     DBs = with pkgs.dictdDBs; [ wiktionary fra2eng eng2fra ];
    # };

    # # wasn't working
    # programs.neovim = {
    #     enable = true;
    #     # package = pkgs.neovim-nightly;
    #     package = pkgs-unstable.neovim;
    #     withNodeJs = true;
    #     withPython3 = true;
    #     withRuby = true;
    #     configure = {
    #         customRC = ''
    #             :luafile ${builtins.getEnv "XDG_CONFIG_HOME" }/nvim/init.lua
    #         '';
    #     };
    # };

}
