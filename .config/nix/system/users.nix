{ pkgs, config, ... }:

{
    # user account
    users.users.felix = {
        isNormalUser = true;
        extraGroups = [ "wheel" "networkmanager" "input" "uinput" ];
    };

    # shell
    programs.zsh.enable = true;
    users.defaultUserShell = pkgs.zsh;

    # home-manager
    imports = [ <home-manager/nixos> ];
    home-manager = {
        useUserPackages = true;
        useGlobalPkgs = true;
        verbose = true;
        users.felix = {pkgs, ...}: {
            home.username = "felix";
            home.homeDirectory = "/home/felix";
            home.stateVersion = "21.11";

            gtk = {
                enable = true;
                cursorTheme.name = "Adwaita";
                cursorTheme.size = 24;
                font.name = "Fira Sans";
                font.size = 11;
                iconTheme.name = "Adwaita";
                theme.name = "SolArc-Dark";
            };

            xdg.userDirs.enable = true;
            xdg.userDirs.createDirectories = true;

            services.udiskie.enable = true;

        };
    };
}
