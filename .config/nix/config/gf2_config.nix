{ config }:

let
  theme = (import ../system/theme.nix) { config = config; };
in
''
  [ui]
  scale=${(toString theme.display_scale)}
  font_size_interface=${(toString (theme.fontmonosize * theme.display_scale))}
  font_size_code=${(toString (theme.fontmonosize * theme.display_scale))}

  [theme]
  panel1=${theme.c00}
  panel2=${theme.cbg}
  text=${theme.cfg}
  textDisabled=${theme.c07}
  border=${theme.c15}
  buttonNormal=${theme.c00}
  buttonHovered=${theme.cbg}
  buttonPressed=${theme.c08}
  buttonDisabled=${theme.c07}
  textboxNormal=${theme.c00}
  textboxFocused=${theme.cbg}
  codeFocused=${theme.cbg}
  codeBackground=${theme.c00}
  codeDefault=${theme.cfg}
  codeComment=${theme.c02}
  codeString=${theme.c01}
  codeNumber=${theme.c06}
  codeOperator=${theme.c06}
  codePreprocessor=${theme.c03}
  selected=${theme.c08}
  textSelected=${theme.cbg}
  accent1=${theme.c01}
  accent2=${theme.c02}
''
