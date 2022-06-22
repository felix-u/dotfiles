#!/usr/bin/env fish

# hide greeting ("Welcome to fish, the friendly inter...")
set fish_greeting

# use vi keybindings
fish_vi_key_bindings

# custom bindings
function fish_user_key_bindings
    # CTRL-n to forward (right arrow also works)
    bind --preset -M insert \cn forward-char
end

# use green, not blue, for valid commands
set -U fish_color_command green
# don't use black background for search matches
set -U fish_color_search_match --background=normal
# WHY IS THIS ONE HARDCODED TO THE UGLIEST YELLOW
set -U fish_pager_color_description blue
# "...and n more rows" text in autocompletion menu
set -U fish_pager_color_progress grey
# colour of suggestion for which to -> or C-n
set -U fish_color_autosuggestion brblack
# bold incorrect commands
set -U fish_color_error red --bold
# normal foreground colour rather than cyan for params
set -U fish_color_param normal
# colour of syntax such as ; and & (default is green)
set -U fish_color_end blue

# set cursor shape for vi modes
set fish_cursor_default     block      blink
set fish_cursor_insert      line       blink
set fish_cursor_replace_one underscore blink
set fish_cursor_visual      block

# source environment variables
source $XDG_CONFIG_HOME/fish/functions/env.fish
# source aliases
source $XDG_CONFIG_HOME/fish/functions/aliases.fish
# source functions
source $XDG_CONFIG_HOME/fish/functions/funcs.fish

# starship prompt
if type -q starship
    starship init fish | source
end

# use in nix-shell
if type -q any-nix-shell
    any-nix-shell fish --info-right | source
end
