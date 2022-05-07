{ pkgs, config, ... }:

{
    # fonts
    fonts.fonts = with pkgs; [
        fira ibm-plex nerdfonts noto-fonts-emoji
    ];
}
