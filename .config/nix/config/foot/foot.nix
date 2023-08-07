let
  theme = import ../../system/theme.nix;
in
{
  main = {
    font = "monospace:regular:size=12";
    font-bold = "monospace:bold:size=12";
    font-italic = "monospace:italic:size=12";
    pad = "25x22";
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
