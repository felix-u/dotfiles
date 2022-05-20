{ pkgs, config, ... }:

{
    # fonts
    fonts.fonts = with pkgs; [

      # actually used
      fira nerdfonts noto-fonts-emoji

      # for curiosity
      cherry cozette creep curie dina-font gohufont scientifica tamsyn ibm-plex
      
    ];
}
