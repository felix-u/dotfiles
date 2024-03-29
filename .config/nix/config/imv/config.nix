let
  theme = import ../../system/theme.nix;
in
''
  # mostly default config

  [options]

  background = #${theme.c08}
  overlay_font = ${theme.fontsans}:12
  overlay_text_color = #${theme.cfg}
  overlay_background_color = #${theme.c00}
  overlay_position_bottom = true

  scaling_mode = shrink

  # suppress default config and rebind here, to make my life easier
  suppress_default_binds = true

  [binds]
  q = quit
  y = exec echo working!

  # image navigation
  <p> = prev
  <Shift+N> = prev
  <n> = next
  gg = goto 1
  <Shift+G> = goto -1

  # Panning
  j = pan 0 -50
  k = pan 0 50
  h = pan 50 0
  l = pan -50 0
  <Down> = pan 0 -50
  <Up> = pan 0 50
  <Left> = pan 50 0
  <Right> = pan -50 0

  # Zooming
  <Shift+plus> = zoom 1
  i = zoom 1
  <minus> = zoom -1
  o = zoom -1

  # Rotate Clockwise by 90 degrees
  <Ctrl+r> = rotate by 90

  # Other commands
  x = close
  f = fullscreen
  d = overlay
  c = center
  s = scaling next
  <Shift+S> = upscaling next
  a = zoom actual
  r = reset

  # Gif playback
  <period> = next_frame
  <space> = toggle_playing

  # Delete and then close an open image by pressing 'X'
  <Shift+X> = exec rm "$imv_current_file"; close
  # Rotate the currently open image by 90 degrees by pressing 'R'
  <Shift+R> = exec mogrify -rotate 90 "$imv_current_file"

  # Slideshow control
  t = slideshow +1
  <Shift+T> = slideshow -1

  # vim:ft=dosini
''
