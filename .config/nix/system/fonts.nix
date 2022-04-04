{ pkgs, config, lib, ... }:

{
    # fonts
    fonts.fonts = with pkgs; [
        fira nerdfonts noto-fonts-emoji
    ];
}
