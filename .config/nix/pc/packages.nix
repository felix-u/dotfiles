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
      config.boot.kernelPackages.amdgpu-pro
      corectrl
      glxinfo
      prismlauncher
      unstable.optifine

    ];
}
