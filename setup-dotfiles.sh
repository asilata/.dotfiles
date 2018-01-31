#!/bin/zsh

DOTFILES=~/.dotfiles

# Emacs
ln -s $DOTFILES/init.el ~/.emacs.d/init.el

# Email stuff
ln -s $DOTFILES/email/mu4e-config.el ~/.emacs.d/opt/mu4e-config.el
ln -s $DOTFILES/email/offlineimaprc ~/.offlineimaprc
ln -s $DOTFILES/email/offlineimap.py ~/.offlineimap/offlineimap.py
ln -s $DOTFILES/email/muttrc ~/.mutt/muttrc
ln -s $DOTFILES/email/mutt_zenburn_colours ~/.mutt/mutt_zenburn_colours

# ZSH
ln -s $DOTFILES/zshrc ~/.zshrc

# WM
# Create the .xmonad directory before running this.
ln -s $DOTFILES/wm/xmonad.hs ~/.xmonad/xmonad.hs

# Gitignore
ln -s $DOTFILES/gitignore_global ~/.gitignore_global

# Emacs
ln -s $DOTFILES/org-mode-config.el ~/.emacs.d/opt/org-mode-config.el

# Sets up everything except tablet scripts. To set up tablet scripts, uncomment the following.
# sudo ln -s $DOTFILES/tablet/cw-rotate /usr/local/bin/cw-rotate
# sudo ln -s $DOTFILES/tablet/toggle-tablet /usr/local/bin/toggle-tablet

