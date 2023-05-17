{ pkgs, config, ... }:

{
    environment.systemPackages =
    let 
        unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
        kmonad = import ../derivations/kmonad.nix;
    in
    with pkgs; [

        # TERMINAL MISC
            #

        # DESKTOP
            # ckan
            kmonad

        # AUDIO/VIDEO
            # davinci-resolve # runs through xwayland :(
            libsForQt5.kdenlive

        # GAMING
            corectrl glxinfo config.boot.kernelPackages.amdgpu-pro
            unstable.optifine prismlauncher

    ];
}
