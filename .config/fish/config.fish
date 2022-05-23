# hide greeting ("Welcome to fish, the friendly inter...")
set fish_greeting

# custom bindings
function fish_user_key_bindings
    # use vi keybindings
    fish_vi_key_bindings

    # # CTRL-n to forward (right arrow also works)
    # bind --preset \cn forward-char

end

# use green, not blue, for valid commands
set -U fish_color_command green

# set cursor shape for vi modes
set fish_cursor_default     block      blink
set fish_cursor_insert      line       blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual      block

# source aliases
source $XDG_CONFIG_HOME/fish/functions/aliases.fish
# source functions
source $XDG_CONFIG_HOME/fish/functions/funcs.fish

# starship prompt
starship init fish | source
