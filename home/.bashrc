# history
bind '"\e[A":history-search-backward'
bind '"\e[B":history-search-forward'

# Environment variables
source ~/.config/zsh/ENV_VARS.sh

# Functions and aliases
source ~/.config/zsh/ALIASES.sh

# Starship prompt
eval "$(starship init bash)"
