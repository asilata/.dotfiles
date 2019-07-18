#!/bin/zsh

DOTFILES=~/.dotfiles

# Emacs
if [ ! -d "~/.emacs.d/"]; then
    mkdir ~/.emacs.d
    mkdir ~/.emacs.d/opt
fi
ln -s $DOTFILES/init.el ~/.emacs.d/init.el
ln -s $DOTFILES/org-mode-config.el ~/.emacs.d/opt/org-mode-config.el

# Email stuff
ln -s $DOTFILES/email/mu4e-config.el ~/.emacs.d/opt/mu4e-config.el
ln -s $DOTFILES/email/mbsyncrc ~/.mbsyncrc

# ZSH
ln -s $DOTFILES/zshrc ~/.zshrc

# WM
if [ ! -d "~/.xmonad"]; then
    mkdir ~/.xmonad
fi
ln -s $DOTFILES/wm/xmonad.hs ~/.xmonad/xmonad.hs

# Gitignore
ln -s $DOTFILES/gitignore_global ~/.gitignore_global

# Sets up everything except tablet scripts. To set up tablet scripts, uncomment the following.
# sudo ln -s $DOTFILES/tablet/cw-rotate /usr/local/bin/cw-rotate
# sudo ln -s $DOTFILES/tablet/toggle-tablet /usr/local/bin/toggle-tablet

