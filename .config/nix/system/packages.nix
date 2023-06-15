{ pkgs, config, lib, ... }:

let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
  flake-compat = builtins.fetchTarball "https://github.com/edolstra/flake-compat/archive/master.tar.gz";
  # hyprland = (import flake-compat {
  #     src = builtins.fetchTarball "https://github.com/hyprwm/Hyprland/archive/master.tar.gz";
  # }).defaultNix;
in
{
  # imports = [
  #     hyprland.nixosModules.default
  # ];
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
  # # NUR
  # nixpkgs.config.packageOverrides = pkgs: {
  #     nur = import
  #         (builtins.fetchTarball
  #             "https://github.com/nix-community/NUR/archive/master.tar.gz") {
  #         inherit pkgs;
  #     };
  # };

  # Overlays
  # services.emacs.package = pkgs.emacsPgtk;
  # services.emacs.enable = false;
  # nixpkgs.overlays = [
  #   (import (builtins.fetchGit {
  #     url = "https://github.com/nix-community/emacs-overlay.git";
  #     ref = "master";
  #     rev = "f438072ac6d2a0271a0ac5ad566d9612a9b35bb9";
  #   }))
  #   hyprland.overlays.default
  #   (self: super: {
  #       waybar = super.waybar.overrideAttrs (oldAttrs: {
  #           mesonFlags = oldAttrs.mesonFlags ++ [ "-Dexperimental=true" ];
  #       });
  #   })
  #   (import (builtins.fetchTarball {
  #     url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
  #   }))
  # ];

  documentation.man.generateCaches = true;

  environment.systemPackages =
    let

      unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

      # "sudo" runs doas
      doas-as-sudo = (pkgs.writeShellScriptBin "sudo" ''
        echo "Warning: \"sudo\" runs \"doas\""
        doas "$@"
      '');

      helix-src = builtins.fetchTarball {
        url = "https://github.com/helix-editor/helix/archive/7f5940be80.tar.gz";
      };
      helix-git = import flake-compat { src = helix-src; };

      odin-dev = pkgs.odin.overrideAttrs (oldAttrs: rec {
        nativeBuildInputs = with pkgs; oldAttrs.nativeBuildInputs ++ [
          which
          llvmPackages.llvm.dev
        ];
        preBuild = ''
          patchShebangs build_odin.sh
        '';
        src = pkgs.fetchFromGitHub {
          owner = "odin-lang";
          repo = "Odin";
          rev = "dev-2023-03";
          sha256 = "sha256-SIU1VZgac0bL6byai2vMvgl3nrWZaU9Hn0wRqazzxn4=";
        };
        installPhase = ''
          mkdir -p $out/bin
          cp odin $out/bin/odin
          cp -r core $out/bin/core
          # CWD="$(pwd)"
          # cd vendor/stb/src && ${pkgs.gnumake}/bin/make
          # cd "$CWD"
          # cp -r vendor $out/bin/vendor
          wrapProgram $out/bin/odin --prefix PATH : ${lib.makeBinPath (with pkgs.llvmPackages; [
            bintools
            llvm
            clang
            lld
          ])}
        '';
      });

      # broken
      nfm = pkgs.callPackage ../derivations/nfm.nix { };

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

      zls-src = builtins.fetchTarball { url = "https://github.com/zigtools/zls/archive/master.tar.gz"; };
      zls-master = import flake-compat { src = zls-src; };

    in
    with pkgs; [

      # ESSENTIAL
      foot
      gh
      git
      neofetch
      nvi
      stow
      wget

      # DEV AND PROGRAMMING
      # misc
      ascii
      bviplus
      gdb
      gnumake
      rr
      valgrind
      # android-tools bsdiff
      # libresprite pixelorama rx
      # openssl_3_0 pkg-config protobuf unstable.godot godot4-alpha
      # c
      binutils-unwrapped-all-targets
      clang
      clang-tools
      cmake
      cppcheck
      gcc
      man-pages-posix
      tinycc
      # cmake-language-server
      # go
      go
      gopls
      # java
      jdk11
      # web (HTML, CSS, JS)
      nodePackages.npm
      nodejs
      yarn
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
      # rnix-lsp
      # # odin
      #     odin-dev # ols
      # plan9 from user space
      plan9port
      # python
      python3Full
      # rust
      cargo
      # clippy rust-analyzer sccache
      # shell
      shellcheck
      # # vim
      #     nodePackages.vscode-langservers-extracted
      #     nodePackages.vim-language-server
      # zig
      zig-master
      # zls-master.defaultNix.packages.x86_64-linux.default
      # MATHS
      bc
      libqalculate
      # maxima octaveFull gnuplot 

      # TERMINAL MISC
      _7zz
      doas-as-sudo
      entr
      fd
      ffmpeg
      figlet
      file
      fzf
      helix-git.defaultNix.packages.x86_64-linux.default
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
      termato
      themesh
      tldr
      tree
      tty-clock
      udiskie
      udisks
      unrar
      unzip
      v4l-utils
      w3m
      xdg-utils
      xdragon
      youtube-dl
      zip

      # MUSIC & AUDIO
      cava
      ncspot
      pavucontrol
      pulsemixer
      # cmus

      # INTERNET & BLUETOOTH
      blueberry
      bluez
      bluez-tools
      firefox
      newsboat

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
      tuner
      wally-cli
      xournalpp
      zathura

      # VISUAL
      # adw-gtk3
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
      jpegoptim
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

}
