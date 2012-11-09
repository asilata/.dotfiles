# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall

zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' menu select=1
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '~/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# Colours in the prompt
autoload colors
colors

# Make dealing with colours easier
for COLOR in RED GREEN YELLOW WHITE BLACK CYAN MAGENTA BLUE; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'        
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done                                                
PR_RESET="%{${reset_color}%}";

# Git information in the prompt.
source ~/.zsh-git-prompt/zshrc.sh 

# Left prompt: Green username@hostname: and yellow current path.
PROMPT="$PR_BRIGHT_GREEN%n@%m:$PR_BRIGHT_YELLOW%~$PR_RESET$ "
# Right prompt: Red current time.
RPROMPT='$(git_super_status)$PR_BRIGHT_RED%t$PR_RESET'

# Don't put duplicate lines in the history.
setopt HIST_IGNORE_DUPS

# Run German fortune
spruch -x && echo "\n"

# Aliases
alias ls='ls --color=auto'
alias 1234='setxkbmap -layout dvorak -option ctrl:swapcaps'
alias 2345='setxkbmap -layout us -option'
alias ec='emacsclient -c -n'
alias et='emacsclient -t'

#ibus
# export XMODIFIERS=@im=ibus    #case matters for this variable!
# export GTK_IM_MODULE=ibus
# export QT_IM_MODULE=ibus

# Better colours in the terminal
export TERM='xterm-256color'

# 10 second wait if you do something that will delete everything.
setopt RM_STAR_WAIT
