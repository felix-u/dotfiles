{ pkgs, config, ... }:

let
    alte-haas-grotesk = import ../derivations/alte-haas-grotesk.nix;
    beteckna = import ../derivations/beteckna.nix;
    league-spartan = import ../derivations/league-spartan.nix;
    lora = import ../derivations/lora.nix;
    metropolis = import ../derivations/metropolis.nix;
    montserrat = import ../derivations/montserrat.nix;
    oswald = import ../derivations/oswald.nix;
    playfair = import ../derivations/playfair.nix;
    space-grotesk = import ../derivations/space-grotesk.nix;
    spacemono = import ../derivations/spacemono.nix;
in {
    # fonts
    fonts.fonts = with pkgs; [

        # essential
        fira nerdfonts noto-fonts-emoji

        # proportional fonts
        aileron open-sans # helvetica but FOSS
        alte-haas-grotesk # helvetica but with a printed appearance. not FOSS
        beteckna league-spartan # futura but FOSS
        crimson # oldstyle serif - FOSS
        eb-garamond # Garamond - FOSS
        fraunces vollkorn # cooper black (ish), but FOSS
        freefont_ttf # FOSS versions of helvetica, times, and courier
        inter # like a mix between roboto and apple san francisco - FOSS
        gyre-fonts # TeX fonts - FOSS
                   # Gyre Schola = Century Expanded, and many other FOSS
                   # imitations of well-known fonts
        liberation_ttf # FOSS versions of arial, times new roman, and courier new
        libre-baskerville  # baskerville - FOSS
        libre-bodoni # bodoni - FOSS
        lora # oldstyle-ish serif - FOSS
        metropolis montserrat # gotham but FOSS
        oswald # classic gothic style sans-serif - FOSS
        playfair # "luxury" serif vaguely resempling bodoni - FOSS
        roboto roboto-slab # Google's helvetica-ish grotesque - FOSS
        source-serif source-serif-pro # classic serifs from adobe - FOSS
        space-grotesk # similar to Futura, but does its own thing.
                      # based on Space Mono - foss
        work-sans # another oldstyle grotesque


        # some more monospace fonts
        office-code-pro ibm-plex roboto-mono
        spacemono # slab monospace font which inspired space-grotesk - FOSS

        # low res bitmap fonts, out of curiosity
        cherry cozette creep curie dina-font gohufont scientifica tamsyn

    ];
}
