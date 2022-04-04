{ pkgs, config, ... }:

{
    # fonts
    fonts.fonts = with pkgs; [
        fira nerdfonts noto-fonts-emoji
    ];
}
