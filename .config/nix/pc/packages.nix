{ pkgs, config, ... }:

{
    environment.systemPackages =
    let unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };
    in
    with pkgs; [

        # TERMINAL MISC
            #

        # DESKTOP
            ckan

        # AUDIO/VIDEO
            davinci-resolve # runs through xwayland :(
            libsForQt5.kdenlive

        # GAMING
            corectrl glxinfo config.boot.kernelPackages.amdgpu-pro openspades

    ];
}
