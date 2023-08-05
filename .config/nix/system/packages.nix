{ pkgs, config, lib, ... }:

let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
in
{
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

  documentation.man.generateCaches = true;

  environment.systemPackages =
    let

      unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

      nota = import ../derivations/nota.nix;

      ols = import ../derivations/ols.nix;

      poop = import ../derivations/poop.nix;

      imgclr = import ../derivations/imgclr.nix;

      shgen = import ../derivations/shgen.nix;

      signal-desktop = pkgs.signal-desktop.overrideAttrs (oldAttrs: rec {
        runtimeDependencies = oldAttrs.runtimeDependencies ++ [ pkgs.wayland ];
      });

      termato = pkgs.callPackage ../derivations/termato.nix { };

      themesh = import ../derivations/themesh.nix;

      wl-screenrec = import ../derivations/wl-screenrec.nix;

      zig-master = import ../derivations/zig-master.nix;

    in
    with pkgs; [

      # ESSENTIAL
      foot
      gh
      git
      nvi
      stow
      wget
      (pkgs.symlinkJoin {
        name = "neofetch";
        paths = [ pkgs.neofetch ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/neofetch --add-flags "--config ${../config/neofetch/config.conf}";
        '';
      })


      # DEV AND PROGRAMMING
      # misc
      ascii
      bviplus
      gdb
      gnumake
      rr
      valgrind
      # c
      binutils-unwrapped-all-targets
      clang
      clang-tools
      cmake
      cppcheck
      gcc
      man-pages-posix
      tinycc
      # go
      go
      # java
      jdk11
      # web (HTML, CSS, JS)
      nodePackages.npm
      nodejs
      # lua
      lua
      # nix
      any-nix-shell
      cachix
      deadnix
      home-manager
      nix-index
      nixpkgs-fmt
      statix
      # plan9 from user space
      plan9port
      # python
      python3Full
      # rust
      cargo
      # shell
      shellcheck
      # zig
      zig-master
      # MATHS
      bc
      libqalculate

      # Utils
      _7zz
      entr
      fd
      ffmpeg
      figlet
      file
      fzf
      glxinfo
      htop
      hunspell
      hunspellDicts.en-gb-ise
      imgclr
      jq
      killall
      libv4l
      lm_sensors
      moreutils
      ncdu
      nota
      nvd
      onefetch
      pandoc
      pastel
      poop
      poppler_utils
      ripgrep
      sdcv
      shgen
      termato
      themesh
      tldr
      tree
      unrar
      unzip
      v4l-utils
      w3m
      xdg-utils
      xdragon
      youtube-dl
      zip

      # MUSIC & AUDIO
      pavucontrol
      pulsemixer

      # INTERNET & BLUETOOTH
      blueberry
      bluez
      bluez-tools

      # DESKTOP
      anki-bin
      appimage-run
      bitwarden
      calibre
      font-manager
      imagemagick
      imv
      libnotify
      libreoffice
      libva
      libva-utils
      mpv
      mpvScripts.youtube-quality
      obs-studio
      obs-studio-plugins.wlrobs
      pantheon.elementary-files
      qt5ct
      signal-desktop
      wally-cli
      xournalpp
      zathura

      # VISUAL
      elementary-xfce-icon-theme
      gnome.adwaita-icon-theme
      gsettings-desktop-schemas
      gtk-engine-murrine
      gtk_engines
      pantheon.elementary-gtk-theme

      # PHOTO, GRAPHICS & VIDEO
      asciinema
      asciinema-agg
      gimp-with-plugins
      hugin
      inkscape-with-extensions
      krita
      luminanceHDR
      mediainfo
      unstable.darktable

      # GAMING
      proton-caller
      protontricks
      protonup

      # LATEX
      biber
      texinfo
      texlab
      texlive.combined.scheme-full

      # KERNEL
      config.boot.kernelPackages.v4l2loopback

    ];

  programs.firefox = {
    enable = true;
    # TODO
    # policies = {};
  };

  programs.neovim = {
    enable = true;
    configure = {
      customRC = builtins.readFile (toString ../config/nvim/init.vim);
      packages.myVimPackage = with pkgs.vimPlugins; {
        start = [
          fzf-vim
          vim-clang-format
          vim-commentary
        ];
      };
    };
    defaultEditor = true;
  };

  # Isn't installed correctly if in package list
  programs.steam.enable = true;

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (toString ../config/tmux/tmux.conf);
  };

}
