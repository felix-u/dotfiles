{ pkgs, ... }:

let

  eunomia = import ../derivations/fonts/dotcolon/eunomia.nix;
  fa-1 = import ../derivations/fonts/dotcolon/fa-1.nix;
  ferrum = import ../derivations/fonts/dotcolon/ferrum.nix;
  medio = import ../derivations/fonts/dotcolon/medio.nix;
  melete = import ../derivations/fonts/dotcolon/melete.nix;
  nacelle = import ../derivations/fonts/dotcolon/nacelle.nix;
  penna = import ../derivations/fonts/dotcolon/penna.nix;
  route159 = import ../derivations/fonts/dotcolon/route159.nix;
  seshat = import ../derivations/fonts/dotcolon/seshat.nix;
  tenderness = import ../derivations/fonts/dotcolon/tenderness.nix;

  alte-haas-grotesk = import ../derivations/fonts/alte-haas-grotesk.nix;
  # apple-fonts       = pkgs.callPackage ../derivations/fonts/apple-fonts.nix {};
  beteckna = import ../derivations/fonts/beteckna.nix;
  chicagoflf = import ../derivations/fonts/chicagoflf.nix;
  fragment-mono = import ../derivations/fonts/fragment-mono.nix;
  league-spartan = import ../derivations/fonts/league-spartan.nix;
  lora = import ../derivations/fonts/lora.nix;
  metropolis = import ../derivations/fonts/metropolis.nix;
  montserrat = import ../derivations/fonts/montserrat.nix;
  oswald = import ../derivations/fonts/oswald.nix;
  playfair = import ../derivations/fonts/playfair.nix;
  sf-mono = import ../derivations/fonts/sfmononerdfont.nix;
  space-grotesk = import ../derivations/fonts/space-grotesk.nix;
  spacemono = import ../derivations/fonts/spacemono.nix;
in
{

  nixpkgs.config.input-fonts.acceptLicense = true;

  # fonts
  fonts = {
    fonts = with pkgs; [

      # essential

      (import ../derivations/fonts/commit-mono.nix)
      eb-garamond
      inter
      nerdfonts
      noto-fonts-emoji

      # https://dotcolon.net

      eunomia
      fa-1
      ferrum
      medio
      melete
      nacelle
      penna
      route159
      seshat
      tenderness

      # proportional

      # Gyre Schola = Century Expanded, and many other FOSS
      # based on Space Mono - foss
      # imitations of well-known fonts
      aileron
      alte-haas-grotesk # helvetica but with a printed appearance. not FOSS
      beteckna
      chicagoflf # retro Apple system font - vectorised
      crimson # oldstyle serif - FOSS
      fraunces
      freefont_ttf # FOSS versions of helvetica, times, and courier
      gyre-fonts # TeX fonts - FOSS
      helvetica-neue-lt-std # actual helvetica
      league-spartan # futura but FOSS
      liberation_ttf # FOSS versions of arial, times new roman, and courier new
      libre-baskerville # baskerville - FOSS
      libre-bodoni # bodoni - FOSS
      lora # oldstyle-ish serif - FOSS
      metropolis
      montserrat # gotham but FOSS
      open-sans # helvetica but FOSS
      oswald # classic gothic style sans-serif - FOSS
      playfair # "luxury" serif vaguely resempling bodoni - FOSS
      roboto
      roboto-slab # Google's helvetica-ish grotesque - FOSS
      source-serif # classic serifs from adobe - FOSS
      source-serif-pro
      space-grotesk # similar to Futura, but does its own thing.
      vollkorn # cooper black (ish), but FOSS
      work-sans # another oldstyle grotesque

      # monospace

      fira
      fragment-mono
      ibm-plex
      iosevka-bin
      iosevka-comfy.comfy-fixed
      jetbrains-mono
      office-code-pro
      roboto-mono
      sf-mono
      spacemono

    ];

    fontconfig = {
      enable = true;
      defaultFonts = let theme = import ./theme.nix; in {
        monospace = [ "${theme.fontmono}" ];
        sansSerif = [ "${theme.fontsans}" ];
        serif = [ "${theme.fontserif}" ];
      };
    };
  };

}
