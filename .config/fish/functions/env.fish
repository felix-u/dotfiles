#!/usr/bin/env fish

set -Ux XRESOURCES "$HOME/.Xresources"
set -Ux DOTFILES "$HOME/dotfiles"
set -Ux DOTSCONF "$DOTFILES/.config"

if type -q fish_add_path
    fish_add_path "$HOME/.local/bin"
    fish_add_path "$XDG_CONFIG_HOME/zsh/scripts-in-path"
end
set -Ux PKG_CONFIG_PATH "/usr/include"
set -Ux BAT_THEME "ansi"

# Defaults
set -Ux TERMINAL 'foot'
set -Ux TERM_ITALICS true
set -Ux EDITOR 'nvim'
set -Ux VISUAL "nvim"
set -Ux READER 'zathura'
set -Ux VIDEO 'mpv'
set -Ux IMAGE 'imv'
set -Ux WM 'sway'
set -Ux PAGER 'less'
set -Ux MANPAGER $PAGER
set -Ux BROWSER "qutebrowser"
set -Ux XCURSOR_SIZE 24

# LESS colours
set -Ux LESS_TERMCAP_mb \e\[1\;36m
set -Ux LESS_TERMCAP_md \e\[1\;34m
set -Ux LESS_TERMCAP_me \e\[32m
set -Ux LESS_TERMCAP_se \e\[0m
set -Ux LESS_TERMCAP_so \e\[1\;31m
set -Ux LESS_TERMCAP_ue \e\[0m
set -Ux LESS_TERMCAP_us \e\[1\;36m

set -Ux QT_QPA_PLATFORMTHEME "qt5ct"

# RUST
set -Ux RUSTC_WRAPPER "sccache"

set -Ux STARSHIP_CONFIG ~/.config/starship/starship.toml

# les graphics
set -Ux AMD_VULKAN_ICD "RADV"
set -Ux SDL_VIDEODRIVER "wayland"

# nnn file manager
set -Ux NNN_FIFO '/tmp/nnn.fifo'
set -Ux NNN_TMPFILE "/tmp/nnn"
set -Ux NNN_COLORS '4444'
set -Ux NNN_TRASH 1
set -Ux NNN_OPTS "C"

# xdg-open and xdg-mime associations
if type -q handlr
    handlr set image/png imv.desktop
    handlr set image/jpeg imv.desktop
    handlr set image/ppm imv.desktop
end

# make cd great again
set cdpath $HOME

# XDG environment variables
set -Ux XDG_CONFIG_HOME "$HOME/.config"
set -Ux XDG_CACHE_HOME "$HOME/.cache"
set -Ux XDG_DATA_HOME "$HOME/.local/share"
set -Ux XDG_STATE_HOME "$HOME/.local/state"
set -Ux XDG_CURRENT_DESKTOP sway

# git config
git config --global core.editor $EDITOR

# wayland
set -Ux NIXOS_OZONE_WL 1
