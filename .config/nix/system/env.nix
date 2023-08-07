{
  environment.sessionVariables = rec {

    FONT_MONO = "CommitMono";
    FONT_SANS = "Inter";
    FONT_SERIF = "EBGaramond12";

    CLR_FG = "000000";
    CLR_BG = "ffffff";
    CLR_00 = "efefef";
    CLR_08 = "c0c0c0";
    CLR_01 = "a0342f";
    CLR_09 = "a0342f";
    CLR_02 = "065905";
    CLR_10 = "065905";
    CLR_03 = "999950";
    CLR_11 = "999950";
    CLR_04 = "007ed6";
    CLR_12 = "007ed6";
    CLR_05 = "8888cc";
    CLR_13 = "8888cc";
    CLR_06 = "57a8a8";
    CLR_14 = "57a8a8";
    CLR_07 = "777777";
    CLR_15 = "000000";

    CLR_FG_ALT = "000000";
    CLR_BG_ALT = "fdffea";
    CLR_00_ALT = "eeee9e";
    CLR_08_ALT = "c1c270";
    CLR_15_ALT = "000000";

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
