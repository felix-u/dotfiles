set fish_greeting

fish_vi_key_bindings

function fish_user_key_bindings
    bind --preset -M insert \cn forward-char
end

set -U fish_color_command normal
set -U fish_color_quote cyan
set -U fish_color_search_match --background=normal
set -U fish_pager_color_description yellow
set -U fish_pager_color_progress grey
set -U fish_color_autosuggestion brblack
set -U fish_color_error red --bold
set -U fish_color_param normal
set -U fish_color_end green

function mkcd
    mkdir -p $argv[1]
    cd $argv[1]
end

if type -q any-nix-shell
    any-nix-shell fish --info-right | source
end

if set -q SSH_CLIENT || set -q SSH_TTY
    set -xg TERM screen-256color
end

function fish_prompt
  set_color cyan --bold && echo (prompt_pwd --full-length-dirs 3)
  set_color normal && set_color --bold && echo -n '% '
end

function fish_mode_prompt
end
