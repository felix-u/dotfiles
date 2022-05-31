{ pkgs, config, lib, ... }:

{
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
    services.emacs.package = pkgs.emacsPgtkNativeComp;
    services.emacs.enable = true;
    nixpkgs.overlays = [
      (import (builtins.fetchGit {
        url = "https://github.com/nix-community/emacs-overlay.git";
        ref = "master";
        rev = "b324b27d58fe93add90d80e081c39d452ae1cb98";
      }))
    ];

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

        themesh = import ../derivations/themesh.nix;

        shgen = import ../derivations/shgen.nix;

    in
    with pkgs; [

        # ESSENTIAL
      foot kitty unstable.gh git neofetch pfetch starship stow wget

      # DEV
      android-tools any-nix-shell bsdiff cargo clang-tools cmake gcc gnumake go
      home-manager
      libresprite libxkbcommon man-pages-posix nix-index
      openssl_3_0 pkg-config protobuf
      python39Packages.bsdiff4 python39Packages.pip
      python3Full python39Packages.python-lsp-server
      python39Packages.termcolor
      so unstable.rustc shellcheck
      unstable.clang unstable.deadnix unstable.godot unstable.statix
      valgrind
      yarn
      godot4-alpha
      # asepriteLatest
      unstable.aseprite-unfree
      # experimenting with lisp:
      cachix clisp emacsPgtkNativeComp emacs-all-the-icons-fonts libvterm-neovim sbcl

      # NEOVIM
      cmake-language-server nodePackages.bash-language-server
      nodePackages.js-beautify
      nodePackages.npm nodePackages.pyright nodePackages.vim-language-server
      nodePackages.vscode-langservers-extracted nodejs rnix-lsp
      rust-analyzer sumneko-lua-language-server tree-sitter
      unstable.neovim

      # MATHS
      bc gnuplot libqalculate maxima qalculate-gtk wxmaxima

      # TERMINAL MISC
      bat catimg cava cmatrix dict doas-as-sudo xdragon entr figlet file
      ffmpeg fzf
      glow handlr htop
      hunspell hunspellDicts.en-gb-ise hyperfine
      lm_sensors lolcat lowdown ncdu ncspot nvd onefetch oneshot pastel # nnn
      pandoc pdftk pipes-rs ripgrep sdcv shgen skim
      termdown themesh tldr tmux tty-clock ttyper
      udiskie udisks unrar unstable.helix unzip
      v4l-utils libv4l
      w3m xdg-utils youtube-dl unstable.ytfzf zip zsh _7zz

      # INTERNET & BLUETOOTH
      blueberry blueman bluez bluez-tools firefox newsboat qutebrowser

      # DESKTOP
      appimage-run
      bitwarden calibre
      handlr imagemagick imv libreoffice
      libnotify libsForQt5.qtstyleplugin-kvantum libva libva-utils
      mpv mpvScripts.youtube-quality
      obs-studio obs-studio-plugins.wlrobs
      pavucontrol profanity pulsemixer signal-desktop
      qt5ct wally-cli
      # xfce.thunar xfce.thunar-archive-plugin
      pcmanfm
      zathura

      # VISUAL
      gsettings-desktop-schemas gtk-engine-murrine gtk_engines
      solarc-gtk-theme unstable.gnome.adwaita-icon-theme

      # PHOTO, GRAPHICS & VIDEO
      unstable.darktable hugin inkscape-with-extensions jpegoptim krita luminanceHDR
      mediainfo unstable.gimp-with-plugins

      # GAMING
      lutris minecraft unstable.optifine minetest
      proton-caller protontricks protonup
      unstable.heroic

      # LATEX
      biber texinfo texlab texlive.combined.scheme-full

      # KERNEL
      config.boot.kernelPackages.v4l2loopback

    ];

    # steam here, not working in packages
    programs.steam.enable = true;

}
