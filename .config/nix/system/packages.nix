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
      bviplus
      gdb
      gf
      gnumake
      nasm
      rr
      tokei
      valgrind
      xxd
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
      (import ../derivations/zig-master.nix)
      # MATHS
      bc
      libqalculate
      qalculate-gtk

      # Utils
      _7zz
      entr
      fd
      ffmpeg
      figlet
      file
      glxinfo
      htop
      hunspell
      hunspellDicts.en-gb-ise
      (import ../derivations/imgclr.nix)
      jq
      killall
      libv4l
      lm_sensors
      moreutils
      ncdu
      (pkgs.callPackage ../derivations/newsraft.nix { })
      (import ../derivations/nota.nix)
      nvd
      pandoc
      pastel
      (import ../derivations/poop.nix)
      poppler_utils
      ripgrep
      sdcv
      (import ../derivations/shgen.nix)
      (pkgs.callPackage ../derivations/termato.nix { })
      (import ../derivations/themesh.nix)
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
      blanket
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

      # LATEX
      biber
      texinfo
      texlab
      texlive.combined.scheme-full

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
