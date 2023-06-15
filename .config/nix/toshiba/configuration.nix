{ config, pkgs, ... }:
let
  homedir = config.home-manager.users.felix.home.homeDirectory;
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in
{
  imports =
    [
      # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      <home-manager/nixos>
    ];


  boot = {
    loader = {
      grub = {
        enable = true;
        version = 2;
        devices = [ "nodev" ];
        efiSupport = true;
        font = "${pkgs.iosevka}/share/fonts/truetype/iosevka-medium.ttf";
        fontSize = 16;
      };
      efi = { canTouchEfiVariables = true; };
    };
    kernelParams = [ "modprobe.blacklist=video" ];
  };


  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
    earlySetup = true;
    keyMap = "us";
  };


  fonts.fonts = with pkgs; [
    fira
    freefont_ttf
    iosevka
  ];


  users.users.felix = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "uinput" ];
  };

  users.defaultUserShell = pkgs.fish;

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    verbose = true;
    users.felix = { pkgs, ... }: {
      home.username = "felix";
      home.homeDirectory = "/home/felix";
      home.stateVersion = "22.05";
      xdg.userDirs.enable = true;
      xdg.userDirs.createDirectories = true;
    };
  };
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = [ "felix" ];
      keepEnv = true;
      persist = true;
    }];
  };


  networking = {
    hostName = "toshiba";
    networkmanager.enable = true;
  };

  environment.systemPackages = with pkgs; [
    gh
    git
    neofetch
    stow
    wget
    vim
    bat
    any-nix-shell
    home-manager

    # NEOVIM
    cmake-language-server
    nodePackages.bash-language-server
    nodePackages.js-beautify
    nodePackages.npm
    nodePackages.pyright
    nodePackages.vim-language-server
    nodePackages.vscode-langservers-extracted
    nodejs
    rnix-lsp
    rust-analyzer
    sumneko-lua-language-server
    tree-sitter
    unstable.neovim
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

}

