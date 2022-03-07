Disclaimer: this repository is mainly for my own use so no "support" will be provided :)

### Usage

Clone the repo. `git` is required.
```sh
git pull https://github.com/felix-u/dots ~/dotfiles
```

GNU `stow` is required to "install" using my symlinking script.
```sh
cd ~/dotfiles/scripts
export XDG_CONFIG_HOME=~/.config
./linkdots.sh
```
