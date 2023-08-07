let
  theme = import ./theme.nix;
in
{
  environment.sessionVariables = rec {

    FONT_MONO = theme.fontmono;
    FONT_SANS = theme.fontsans;
    FONT_SERIF = theme.fontserif;

    CLR_FG = theme.cfg;
    CLR_BG = theme.cbg;
    CLR_00 = theme.c00;
    CLR_08 = theme.c08;
    CLR_01 = theme.c01;
    CLR_09 = theme.c09;
    CLR_02 = theme.c02;
    CLR_10 = theme.c10;
    CLR_03 = theme.c03;
    CLR_11 = theme.c11;
    CLR_04 = theme.c04;
    CLR_12 = theme.c12;
    CLR_05 = theme.c05;
    CLR_13 = theme.c13;
    CLR_06 = theme.c06;
    CLR_14 = theme.c14;
    CLR_07 = theme.c07;
    CLR_15 = theme.c15;

    CLR_FG_ALT = theme.afg;
    CLR_BG_ALT = theme.abg;
    CLR_00_ALT = theme.a00;
    CLR_08_ALT = theme.a08;
    CLR_15_ALT = theme.a15;

    AGENDAFILE = "$HOME/uni/2023/autumn/agenda";
    AMD_VULKAN_ICD = "RADV";
    BROWSER = "firefox";
    CC = "zig cc";
    DOTFILES = "$HOME/dotfiles";
    DOTSCONF = "$DOTFILES/.config";
    EDITOR = "nvim";
    FILES = "io.elementary.files --new-window";
    FZF_DEFAULT_COMMAND = "fd";
    FZF_DEFAULT_OPTS = "--color=16";
    GREP_COLORS = "ms=1;97";
    IMAGE = "imv";
    MANWIDTH = "80";
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
    PAGER = "less";
    PATH = "$HOME/.local/bin/:$XDG_CONFIG_HOME/sh/scripts-in-path/:$PATH";
    PKG_CONFIG_PATH = "/usr/include";
    PROMPTCHAR = "%";
    PS1 = "$PROMPTCHAR ";
    QT_QPA_PLATFORM = "wayland-egl";
    READER = "zathura";
    RUSTC_WRAPPER = "sccache";
    SDL_VIDEODRIVER = "wayland";
    TERMINAL = "foot";
    TERM_ITALICS = "true";
    VIDEO = "mpv";
    VISUAL = "nvim";
    XCURSOR_SIZE = "24";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CURRENT_DESKTOP = "sway";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_STATE_HOME = "$HOME/.local/state";
  };
}
