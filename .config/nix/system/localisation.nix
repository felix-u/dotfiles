{ pkgs, config, ... }:

{
    # time and internationalisation
    time.timeZone = "America/New_York";
    i18n.defaultLocale = "en_GB.UTF-8";
    # larger font, because 4K
    console = {
        earlySetup = true;
        font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
        packages = with pkgs; [ terminus_font ];
        keyMap = "us";
    };
}
