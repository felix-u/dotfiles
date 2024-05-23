{ config }:

let
  theme = (import ../../system/theme.nix) { config = config; };
in
''
  set guioptions s
  set selection-clipboard clipboard
  set font                        "${theme.fontsans} Bold ${toString theme.fontsanssize}"

  set notification-error-fg       "#${theme.c01}"
  set notification-warning-fg     "#${theme.c03}"
  set notification-fg             "#${theme.c02}"
  set inputbar-fg                 "#${theme.c12}"
  set highlight-color             "#${theme.c03}"
  set highlight-active-color      "#${theme.c04}"

  set render-loading              true
  set recolor                     "false"
  # setting recolor-keep true will keep any color your pdf has.
  # if it is false, it'll just be black and white
  set recolor-keephue             "false"

  set notification-error-bg       "#${theme.cbg}"
  set notification-warning-bg     "#${theme.cbg}"
  set notification-bg             "#${theme.cbg}"

  set completion-bg               "#${theme.c00}"
  set completion-fg               "#${theme.c15}"
  set completion-group-bg         "#${theme.c00}"
  set completion-group-fg         "#${theme.c15}"
  set completion-highlight-bg     "#${theme.c08}"
  set completion-highlight-fg     "#${theme.c15}"

  set index-bg                    "#${theme.cbg}"
  set index-fg                    "#${theme.c15}"
  set index-active-bg             "#${theme.c08}"
  set index-active-fg             "#${theme.c15}"

  set inputbar-bg                 "#${theme.c00}"
  set statusbar-bg                "#${theme.c00}"
  set statusbar-fg                "#${theme.c15}"

  set default-bg                  "#${theme.c00}"
  set default-fg                  "#${theme.cfg}"
  set render-loading-bg           "#${theme.cbg}"
  set render-loading-fg           "#${theme.cfg}"

  set recolor-lightcolor          "#${theme.cbg}"
  set recolor-darkcolor           "#${theme.cfg}"


  # light theme binding (press 1)
  map '1' feedkeys ":set statusbar-bg '#${theme.c00}'<Return>3"
  map '3' feedkeys ":set statusbar-fg '#${theme.cfg}'<Return>4"
  map '4' feedkeys ":set default-bg '#${theme.cbg}'<Return>5"
  map '5' feedkeys ":set default-fg '#${theme.cfg}'<Return>6"
  map '6' feedkeys ":set render-loading-bg '#${theme.cbg}'<Return>7"
  map '7' feedkeys ":set render-loading-fg '#${theme.cfg}'<Return>8"
  map '8' feedkeys ":set recolor-lightcolor '#${theme.cbg}'<Return>9"
  map '9' feedkeys ":set recolor-darkcolor '#${theme.cfg}'<Return>"

  # dark theme binding (press 2)
  map '2' feedkeys ":set statusbar-bg '#${theme.a00}'<Return>0"
  map '0' feedkeys ":set statusbar-fg '#${theme.a15}'<Return>w"
  map 'w' feedkeys ":set default-bg '#${theme.abg}'<Return>f"
  map 'f' feedkeys ":set default-fg '#${theme.afg}'<Return>p"
  map 'p' feedkeys ":set render-loading-bg '#${theme.abg}'<Return>b"
  map 'b' feedkeys ":set render-loading-fg '#${theme.afg}'<Return>u"
  map 'u' feedkeys ":set recolor-lightcolor '#${theme.abg}'<Return>y"
  map 'y' feedkeys ":set recolor-darkcolor '#${theme.afg}'<Return>"
''
