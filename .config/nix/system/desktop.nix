{ pkgs, config, ... }:

let
    dmenu-wl_run = import ../derivations/dmenu-wl.nix;
in {
    # wayland schtuff
    programs.sway = {
        enable = true;
        wrapperFeatures.gtk = true;
        extraPackages = with pkgs; [
            brightnessctl dmenu-wayland dmenu-wl_run dunst flashfocus grim slurp
            swaybg swayidle swaylock waybar wf-recorder wl-clipboard xwayland
        ];
    };
    programs.qt5ct.enable = true;


    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };


    # enable CUPS for printing
    services.printing.enable = true;

}