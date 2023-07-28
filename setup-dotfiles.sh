#!/bin/zsh

# * Initialise
DOTFILES=~/.dotfiles

# * Emacs
if [ ! -d "$HOME/.emacs.d/" ]; then
    mkdir ~/.emacs.d
    mkdir ~/.emacs.d/opt
fi

# * Email stuff
ln -s $DOTFILES/email/mbsyncrc ~/.mbsyncrc

# * ZSH etc
ln -s $DOTFILES/zshrc ~/.zshrc
if [ ! -d "$HOME/.config/"]; then
    mkdir ~/.config/
fi
ln -s $DOTFILES/starship.toml ~/.config/starship.toml

# * Gitignore
git config --global core.excludesfile "$HOME/.gitignore_global"
ln -s $DOTFILES/gitignore_global ~/.gitignore_global


