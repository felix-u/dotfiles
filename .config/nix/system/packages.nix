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
        autoOptimiseStore = true;
        checkConfig = true;
        gc = {
            automatic = true;
            persistent = true;
            dates = "weekly";
            options = "--delete-older-than 30d";
        };
        optimise.automatic = true;
        trustedUsers = [ "root" "felix" ];
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
    # services.emacs.package = pkgs.emacsPgtkNativeComp;
    # services.emacs.enable = true;
    nixpkgs.overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        ref = "master";
        rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
      }))
        hyprland.overlays.default
        (self: super: {
            waybar = super.waybar.overrideAttrs (oldAttrs: {
                mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
            });
        })
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
        }))
    ];

    programs.hyprland = {
        enable = false;
        package = pkgs.hyprland;
    };

    # packages for all systems
    environment.systemPackages =
    let

        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

        dmux = (pkgs.callPackage ../derivations/dmux.nix {});

        # effectively "symlink" sudo to doas
        doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
            echo "Warning: \"sudo\" runs \"doas\""
            doas "$@"
        '');

        # flake-compat = builtins.fetchTarball {
        #     url = "https://github.com/edolstra/flake-compat/archive/b4a34015c698c7793d592d66adbab377907a2be8.tar.gz";
        #     sha256 = "sha256:1qc703yg0babixi6wshn5wm2kgl5y1drcswgszh4xxzbrwkk9sv7";
        # };

        godot4-alpha = import ../derivations/godot4alpha.nix;

        # # this works, but goxel's UI ends up far too small
        # goxel-wayland = pkgs.goxel.overrideAttrs (oldAttrs: rec {
        #     buildInputs = with pkgs; [ glfw-wayland gtk3 libpng12 ];
        # });

        helix-src = builtins.fetchTarball {
            url = "https://github.com/helix-editor/helix/archive/718c3baebecf4a970bc32724c564fa506ed40065.tar.gz";
            sha256 = "sha256:1c08nbdi6bgnhpc1clqy3swphnwj4jqgcm82g0n7ivsakz58nk4k";
        };
        helix-git = import flake-compat { src = helix-src; };

        # hyprland-src = builtins.fetchTarball {
        #     url = "https://github.com/hyprwm/Hyprland/archive/1626707b7f4fd3d2b313e78cb3a41783f072f73b.tar.gz";
        #     sha256 = "sha256:0lmka2724m0ylsmwd9zkrv8bvhhrn7jvrznn50qqbnp42h27dw11";
        # };
        # hyprland-git = (pkgs.callPackage [ import flake-compat { src = hyprland-src; } {} ]);

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
                rev = "dev-2022-08";
                sha256 = "sha256-SLzqb3hOwRhFuGT7JQRWoBZ81giLwV6Qeb+u3T0JAJQ=";
            };
            installPhase = ''
                mkdir -p $out/bin
                cp odin $out/bin/odin
                cp -r core $out/bin/core
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
        foot unstable.gh git neofetch pfetch starship stow wezterm wget

        # UTILS IN RUST
        bat # cat
        bottom # top
        fd # find
        helix-git.defaultNix.packages.x86_64-linux.default
        imgclr # haha, c'est a moi :D
        lsd # ls
        pipes-rs # pipes
        procs # ps
        ripgrep # grep
        skim # fzf
        ttyper

        # DEV
        # c
        unstable.clang clang-tools cppcheck cmake gcc gdb gnumake
        man-pages-posix tinycc valgrind
        # # lisp
        # # clisp emacsPgtkNativeComp emacs-all-the-icons-fonts libvterm-neovim sbcl
        # lua
        lua vscodium-fhs
        # nix
        any-nix-shell cachix unstable.deadnix nix-index unstable.statix
        # odin
        odin-dev ols
        # rust
        cargo cargo-flamegraph clippy rustc sccache
        # zig
        zig-master
        # other
        android-tools bsdiff
        go
        home-manager
        # libresprite
        pixelorama rx
        libxkbcommon mold
        openssl_3_0 pkg-config protobuf
        python39Packages.bsdiff4 python39Packages.pip
        python3Full python39Packages.python-lsp-server
        python39Packages.termcolor
        so shellcheck
        unstable.godot
        yarn
        godot4-alpha goxel

        # NEOVIM
        cmake-language-server nodePackages.bash-language-server
        nodePackages.js-beautify
        nodePackages.npm nodePackages.pyright nodePackages.vim-language-server
        nodePackages.vscode-langservers-extracted
        # nodejs
        rnix-lsp
        rust-analyzer sumneko-lua-language-server tree-sitter

        # MATHS
        bc gnuplot libqalculate maxima qalculate-gtk wxmaxima

        # TERMINAL MISC
        catimg cava cmatrix dict doas-as-sudo xdragon entr figlet file
        ffmpeg fzf
        glow handlr htop
        hunspell hunspellDicts.en-gb-ise hyperfine killall
        lm_sensors lolcat lowdown unstable.lynis ncdu ncspot nvd onefetch oneshot pastel
        pandoc pdftk poppler_utils sdcv shgen
        termdown themesh tldr tmux tty-clock
        udiskie udisks unrar unzip
        v4l-utils libv4l
        w3m xdg-utils youtube-dl unstable.ytfzf zip zsh _7zz

        # INTERNET & BLUETOOTH
        blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

        # DESKTOP
        appimage-run anki-bin
        bitwarden calibre font-manager
        handlr
        # hyprland-git
        hyprland
        # unstable.hyprland
        imagemagick imv libreoffice
        libnotify libsForQt5.qtstyleplugin-kvantum libva libva-utils
        mpv mpvScripts.youtube-quality
        obs-studio obs-studio-plugins.wlrobs
        pavucontrol profanity pulsemixer signal-desktop
        qt5ct wally-cli
        # xfce.thunar xfce.thunar-archive-plugin
        pcmanfm
        waybar zathura

        # VISUAL
        gsettings-desktop-schemas gtk-engine-murrine gtk_engines
        solarc-gtk-theme unstable.gnome.adwaita-icon-theme

        # PHOTO, GRAPHICS & VIDEO
        unstable.darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
        mediainfo unstable.gimp-with-plugins

        # GAMING
        lutris minecraft unstable.optifine minetest
        proton-caller protontricks protonup
        unstable.heroic mangohud
        # retro
        (retroarchFull.overrideAttrs (oldAttrs: {
            cores = oldAttrs.cores ++ [
                libretro.tic80
            ];
        }))
        vice # commodore emulation
        w4 # wasm-4 fantasy console

        # LATEX
        biber texinfo texlab texlive.combined.scheme-full

        # KERNEL
        config.boot.kernelPackages.v4l2loopback

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

    programs.neovim = {
        enable = true;
        package = pkgs.neovim-nightly;
        withNodeJs = true;
        withPython3 = true;
        withRuby = true;
        configure = {
            customRC = ''
                :luafile ${builtins.getEnv "XDG_CONFIG_HOME" }/nvim/init.lua
            '';
        #     packages.myVimPackage = with pkgs-unstable.vimPlugins; {
        #         # loaded on launch
        #         start = [ packer-nvim ];
        #         # manually loadable
        #         # opt = [  ];
        #     };
        };
    };

}
