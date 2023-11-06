let
  theme = import ../../system/theme.nix;
in
''
  width = 200
  height = 25
  anchor = top right

  margin = 5
  bar_padding = 5
  border_offset = 0
  border_size = 3

  border_color = ${theme.cfg}ff
  background_color = ${theme.cbg}ff
  bar_color = ${theme.cfg}ff

  overflow_border_color = ${theme.cfg}ff
  overflow_background_color = ${theme.cbg}ff
  overflow_bar_color = ${theme.c01}ff
''
