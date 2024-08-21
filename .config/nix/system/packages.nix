{ pkgs, config, lib, ... }:

let
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

  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [ "electron-24.8.6" ];
  };

  documentation.man.generateCaches = true;

  environment.systemPackages =
    let
      unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

      # ESSENTIAL
      gh
      git
      nvi
      stow
      wget
      neofetch

      # DEV AND PROGRAMMING
      # misc
      ascii
      blink
      bviplus
      gdb
      gf
      gnumake
      nasm
      ninja
      rr
      tokei
      valgrind
      xxd
      # c
      binutils-unwrapped-all-targets
      clang
      clang-tools
      cmake
      cosmocc
      cppcheck
      gcc
      man-pages-posix
      tinycc
      # go
      go
      # java
      jdk11
      plantuml
      # web (HTML, CSS, JS)
      nodePackages.npm
      nodejs
      # lisp
      sbcl
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
      # sql
      sqlite
      # zig
      (import ../derivations/zig-master.nix)
      # MATHS
      bc
      libqalculate

      # Utils
      (import ../derivations/imgclr.nix)
      (import ../derivations/nota.nix)
      (import ../derivations/poop.nix)
      (import ../derivations/shgen.nix)
      (import ../derivations/themesh.nix)
      (import ../derivations/truly.nix)
      (pkgs.callPackage ../derivations/newsraft.nix { })
      (pkgs.callPackage ../derivations/termato.nix { })
      _7zz
      entr
      fd
      ffmpeg-full
      figlet
      file
      glxinfo
      htop
      hunspell
      hunspellDicts.en-gb-ise
      jq
      killall
      libv4l
      lm_sensors
      moreutils
      ncdu
      nvd
      pandoc
      pastel
      poppler_utils
      qemu_full
      ripgrep
      sdcv
      tldr
      tree
      twitch-dl
      unrar
      unzip
      v4l-utils
      w3m
      xdg-utils
      xdragon
      yt-dlp
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
      blanket
      calibre
      font-manager
      gnome.gnome-maps
      imagemagick
      libnotify
      libreoffice
      libva
      libva-utils
      mpv
      mpvScripts.quality-menu
      obs-studio
      obs-studio-plugins.wlrobs
      obs-studio-plugins.obs-vaapi
      obs-studio-plugins.obs-vkcapture
      qt5ct
      (pkgs.signal-desktop.overrideAttrs (oldAttrs: rec {
        runtimeDependencies = oldAttrs.runtimeDependencies ++ [ pkgs.wayland ];
      }))
      wally-cli
      xournalpp

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

      # LATEX/TYPST
      biber
      texinfo
      texlab
      texlive.combined.scheme-full
      typst

      # KERNEL
      config.boot.kernelPackages.v4l2loopback

    ];

  # Isn't installed correctly if in package list
  programs.steam.enable = true;

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile (toString ../config/tmux/tmux.conf);
  };

}
