# many of the snippets in this config are taken from
# https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_4

{ config, pkgs, ... }:
let
    homedir = config.home-manager.users.felix.home.homeDirectory;
    unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
in {
  imports =
    [ # Include the results of the hardware scan.
      # /etc/nixos/hardware-configuration.nix
      ../hardware-configuration.nix
      <home-manager/nixos>
    ];


  boot = {
      kernelPackages = pkgs.linuxPackages_rpi4;
      tmpOnTmpfs = true;
      initrd.availableKernelModules = [ "usbhid" "usb_storage" ];
      kernelParams = [
          "8250.nr_uarts=1"
          "console=ttyAMA0,115200"
          "console=tty1"
          # A lot GUI programs need this, nearly all wayland applications
          "cma=128M"
      ];
      loader = {
          raspberryPi = {
              enable = true;
              version = 4;
          };
		  grub.enable = false;
      };
  };

  # # required for wireless firmware
  # hardware.enableRedistributableFirmware = true;

  time.timeZone = "America/New_York";
  i18n.defaultLocale = "en_GB.UTF-8";
  console = {
      earlySetup = true;
	  keyMap = "us";
  };

  fonts.fonts = with pkgs; [
	  fira
	  freefont_ttf iosevka
  ];

  # add gpio group
  users.groups.gpio = {};
  # udev rule for gpio
    services.udev.extraRules = ''
      SUBSYSTEM=="bcm2835-gpiomem", KERNEL=="gpiomem", GROUP="gpio",MODE="0660"
      SUBSYSTEM=="gpio", KERNEL=="gpiochip*", ACTION=="add", RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio  /sys/class/gpio/export /sys/class/gpio/unexport ; chmod 220 /sys/class/gpio/export /sys/class/gpio/unexport'"
      SUBSYSTEM=="gpio", KERNEL=="gpio*", ACTION=="add",RUN+="${pkgs.bash}/bin/bash -c 'chown root:gpio /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value ; chmod 660 /sys%p/active_low /sys%p/direction /sys%p/edge /sys%p/value'"
    '';
  users.users.felix = {
      isNormalUser = true;
	  extraGroups = [ "wheel" "networkmanager" "input" "uinput" "gpio" ];
  };

  users.defaultUserShell = pkgs.fish;

  home-manager = {
      useUserPackages = true;
	  useGlobalPkgs = true;
      verbose = true;
	  users.felix = {pkgs, ...}: {
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
      hostName = "nixos-raspi-4";
      networkmanager.enable = true;
  };

  nix = {
      autoOptimiseStore = true;
      gc = {
          automatic = true;
          dates = "weekly";
          options = "--delete-older-than 30d";
      };
      # free up to 1GiB whenevr there is less than 100MiB left
      extraOptions = ''
          min-free = ${toString (100 * 1024 * 1024)}
          max-free = ${toString (1024 * 1024 * 1024)}
      '';
  };
  environment.systemPackages = with pkgs; [

    	gh git neofetch stow wget vim
	    bat unstable.helix
	    any-nix-shell home-manager

	    # # NEOVIM
	    # cmake-language-server nodePackages.bash-language-server
	    # nodePackages.js-beautify nodePackages.npm nodePackages.pyright
	    # nodePackages.vim-language-server nodePackages.vscode-langservers-extracted
	    # nodejs rnix-lsp rust-analyzer sumneko-lua-language-server tree-sitter
	    # unstable.neovim

  ];


  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?
}
