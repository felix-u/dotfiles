{ config }:

let
  theme = (import ../../system/theme.nix) { config = config; };
in
{
  main = {
    font = "monospace:regular:size=${toString theme.fontmonosize}";
    font-bold = "monospace:bold:size=${toString theme.fontmonosize}";
    font-italic = "monospace:italic:size=${toString theme.fontmonosize}";
    pad = "${toString (theme.fontmonosize * 2)}x${toString (theme.fontmonosize * 2)}";
    resize-delay-ms = 0;
  };
  cursor = {
    color = "${theme.cbg} ${theme.cfg}";
    style = "block";
  };
  colors = {
    alpha = 1;
    foreground = "${theme.cfg}";
    background = "${theme.cbg}";
    regular0 = "${theme.c00}";
    regular1 = "${theme.c01}";
    regular2 = "${theme.c02}";
    regular3 = "${theme.c03}";
    regular4 = "${theme.c04}";
    regular5 = "${theme.c05}";
    regular6 = "${theme.c06}";
    regular7 = "${theme.c07}";
    bright0 = "${theme.c08}";
    bright1 = "${theme.c09}";
    bright2 = "${theme.c10}";
    bright3 = "${theme.c11}";
    bright4 = "${theme.c12}";
    bright5 = "${theme.c13}";
    bright6 = "${theme.c14}";
    bright7 = "${theme.c15}";
  };
}
