# Check for an interactive session
[ -z "$PS1" ] && return

# General
TERM=xterm #force 16 bit color mode
platform=`uname`

alias ll='ls -Al'
alias e='emacs -nw'
alias E='sudo emacs -nw'
alias grep='grep --color'
alias sudo='sudo '

if [[ "$platform" == 'Linux' ]]; then
    alias ls='ls --color=auto'
elif [[ "$platform" == 'Darwin' ]]; then
    alias ls='ls -G'
    export CLICOLOR=1
    export LSCOLORS=ExFxCxDxBxegedabagacad
fi

# Navigation
alias ..='cd ..'
alias -- -='cd -'

# Arch Linux specific aliases

if [ -r /etc/issue ]; then
    if [ `grep Arch /etc/issue | wc -l` -gt 0 ]; then
        alias pacman='sudo pacman'
        alias trizen='MAKEFLAGS=j4 trizen --aur --noedit'
    fi
fi

# bind ctrl-r/s to up/down
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'
bind '"\e[5C": forward-word'
bind '"\e[5D": backward-word'

calc() { echo "$@" | bc -l; }
note() { echo "$@" >> ~/Documents/notes; }

# Editors
export EDITOR='vim' #'emacsclient -tty --socket-name /tmp/emacs1000/server'
export ALTERNATE_EDITOR='vim'
export SVN_EDITOR=EDITOR

# Paths
export CUDADIR=/usr/local/cuda
export GAMEDIR=/media/share/games
export BINS=$CUDADIR/bin:$GAMEDIR/bin
export PATH=$PATH:$BINS

# GIT PS1 and auto complete

if [[ "$platform" == 'Darwin' ]]; then
     if [ -r /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
    . /usr/local/etc/bash_completion.d/git-prompt.sh
    . /usr/local/etc/bash_completion.d/git-completion.bash
    fi
elif [ -r /usr/share/git ]; then
    . /usr/share/git/completion/git-completion.bash
    . /usr/share/git/git-prompt.sh
elif [ -r /etc/bash_completion.d/git ]; then
    . /etc/bash_completion.d/git
    if [ -r /usr/share/git-core/contrib/completion/git-prompt.sh ]; then
        . /usr/share/git-core/contrib/completion/git-prompt.sh
    fi
elif [ -r /etc/bash_completion.d/git-prompt ]; then
    . /etc/bash_completion.d/git-prompt
else
    function __git_ps1() { true; }
fi

# PS1 configuration
if [ -r ~/.ps1 ]; then . ~/.ps1; fi

# Machine specific config
if [ -r ~/.bashrc_local ]; then . ~/.bashrc_local; fi

# dottools: add distribution binary directories to PATH
if [ -r "$HOME/.tools-cache/setup-dottools-path.sh" ]; then
  . "$HOME/.tools-cache/setup-dottools-path.sh"
fi
