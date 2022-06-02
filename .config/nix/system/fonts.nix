{ pkgs, config, ... }:

let
    alte-haas-grotesk = import ../derivations/alte-haas-grotesk.nix;
in {
    # fonts
    fonts.fonts = with pkgs; [

        # essential
        fira nerdfonts noto-fonts-emoji

        # proportional fonts
        aileron alte-haas-grotesk eb-garamond freefont_ttf gyre-fonts junicode
        liberation_ttf libre-baskerville open-sans roboto roboto-slab

        # some more monospace fonts
        office-code-pro ibm-plex roboto-mono

        # low res bitmap fonts, out of curiosity
        cherry cozette creep curie dina-font gohufont scientifica tamsyn
        ibm-plex

    ];
}
