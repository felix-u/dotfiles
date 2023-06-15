{ pkgs, config, ... }:

let kmonad = import ../derivations/kmonad.nix;
in {
  environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

      # ESSENTIAL
      kmonad

      # TERMINAL MISC
      tlp

      # DESKTOP
      brightnessctl
      unstable.rnote
      zoom-us

    ];
}
