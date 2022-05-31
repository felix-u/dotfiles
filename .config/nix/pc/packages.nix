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
        libsForQt5.kdenlive

        # GAMING
        corectrl glxinfo
        config.boot.kernelPackages.amdgpu-pro
        # unstable.linuxKernel.packages.linux_5_17.amdgpu-pro
        openspades

    ];
}
