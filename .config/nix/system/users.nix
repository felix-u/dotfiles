{ pkgs, config, ... }:


let
  pkgs-unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

  vim-cutlass = pkgs.vimUtils.buildVimPlugin {
    name = "vim-cutlass";
    src = pkgs.fetchFromGitHub {
      owner = "svermeulen";
      repo = "vim-cutlass";
      rev = "7afd649415541634c8ce317fafbc31cd19d57589";
      sha256 = "sha256-j5W9q905ApDf3fvCIS4UwyHYnEZu5Ictn+6JkV/xjig=";
    };
  };

in
{

  imports = [ <home-manager/nixos> ];

  # user account
  users.users.felix = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "uinput" "sway" ];
  };

  # shell
  programs.bash.vteIntegration = true;
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;

  # home-manager
  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    verbose = true;
    users.felix = { pkgs, ... }: {
      home.username = "felix";
      home.homeDirectory = "/home/felix";
      home.stateVersion = "21.11";

      gtk = {
        enable = true;
        cursorTheme.name = "Adwaita";
        cursorTheme.size = 24;
        font.name = "Inter Medium";
        font.size = 12;
        iconTheme.name = "elementary";
        theme.name = "io.elementary.stylesheet.slate";
        gtk3.extraConfig = {
          gtk-decoration-layout = "menu:";
        };
      };

      xdg.userDirs.enable = true;
      xdg.userDirs.createDirectories = true;

      services.udiskie.enable = true;

      programs.neovim = {
        enable = true;
        package = pkgs-unstable.neovim-unwrapped;
        plugins = with pkgs.vimPlugins; [
          fzf-vim
          vim-commentary # comment stuff out 
          vim-cutlass # Plugin that adds a 'cut' operation separate from 'delete' 
          vim-lastplace # Intelligently reopen files at your last edit position in Vim. 
          vim-repeat # enable repeating supported plugin maps with "."
          vim-unimpaired # Pairs of handy bracket mappings 
          vimtex # filetype plugin for LaTeX files. 

          # # auto-pairs # insert or delete brackets, parens, quotes in pairs 
          # # targets-vim # provides additional text objects 
          # # vim-surround # Delete/change/add parentheses/quotes/XML-tags/much more with ease 
        ];
      };

      programs.chromium = {
        enable = true;
        package = pkgs.chromium;
        commandLineArgs = [
          "-animation-duration-scale=0.5"
          "-enable-features=UseOzonePlatform"
          "-ozone-platform=wayland"
          "file"
        ];
        extensions = [
          # ublock origin
          { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; }
          # surfingkeys
          { id = "gfbliohnnapiefjpjlpjnehglfpaknnc"; }
          # dark reader
          { id = "eimadpbcbfnmbkopoojfekhnkhdbieeh"; }
          # decentraleyes
          { id = "ldpochfccmkkmhdbclfhpagapcfdljkj"; }
          # stylus
          { id = "clngdbkpkpeebahjckkjfobafhncgmne"; }
        ];
      };

      # w3m config
      home.file.".w3m/config".text = ''
        confirm_qq false
      '';

    };

  };

  # doas instead of sudo (why not)
  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = [ "felix" ];
      keepEnv = true;
      persist = true;
    }];
  };

}
