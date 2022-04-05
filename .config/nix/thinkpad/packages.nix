{ pkgs, config, ... }:

let kmonad = import ../derivations/kmonad.nix;
in {
    environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

        # TERMINAL MISC
        tlp

        # DESKTOP
        unstable.rnote xournalpp

    ];
}
