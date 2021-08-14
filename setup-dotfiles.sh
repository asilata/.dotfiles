#!/bin/zsh

# * Initialise
DOTFILES=~/.dotfiles

# * Emacs
if [ ! -d "$HOME/.emacs.d/" ]; then
    mkdir ~/.emacs.d
    mkdir ~/.emacs.d/opt
fi
ln -s $DOTFILES/init.el ~/.emacs.d/init.el
ln -s $DOTFILES/org-mode-config.el ~/.emacs.d/opt/org-mode-config.el

# * Email stuff
ln -s $DOTFILES/email/mu4e-config.el ~/.emacs.d/opt/mu4e-config.el
ln -s $DOTFILES/email/mbsyncrc ~/.mbsyncrc

# * ZSH etc
ln -s $DOTFILES/zshrc ~/.zshrc
if [ ! -d "$HOME/.config/"]; then
    mkdir ~/.config/
fi
ln -s $DOTFILES/starship.toml ~/.config/starship.toml

# * WM
if [ ! -d "$HOME/.xmonad" ]; then
    mkdir ~/.xmonad
fi
ln -s $DOTFILES/wm/xmonad.hs ~/.xmonad/xmonad.hs
ln -s $DOTFILES/wm/exwm-config-file.el ~/.emacs.d/opt/exwm-config-file.el

# * Gitignore
git config --global core.excludesfile "$HOME/.gitignore_global"
ln -s $DOTFILES/gitignore_global ~/.gitignore_global

# * Tablet scripts
# sudo ln -s $DOTFILES/tablet/cw-rotate /usr/local/bin/cw-rotate
# sudo ln -s $DOTFILES/tablet/toggle-tablet /usr/local/bin/toggle-tablet

