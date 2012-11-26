# Check for an interactive session
[ -z "$PS1" ] && return

# General
alias ls='ls --color=auto'
alias ll='ls -Al'
alias e='emacs -nw'
alias E='sudo emacs -nw'
alias grep='grep --color'
alias sudo='sudo '

# Arch Linux specific aliases
if [ `grep Arch /etc/issue | wc -l` -gt 0 ]; then
    alias pacman='sudo pacman-color'
    alias pacaur='pacaur --aur --noedit'
fi

# bind ctrl-r/s to up/down
bind '"\e[A": history-search-backward'
bind '"\e[B": history-search-forward'

calc() { echo "$@" | bc -l; }
note() { echo "$@" >> ~/Documents/notes; }

# Editors
export EDITOR='emacs -nw'
export SVN_EDITOR=EDITOR

# Paths
export CUDADIR=/usr/local/cuda
export GAMEDIR=/media/share/games
export BINS=$CUDADIR/bin:$GAMEDIR/bin
export PATH=$PATH:$BINS

# GIT PS1 and auto complete
if [ -r /usr/share/git ]; then
    . /usr/share/git/completion/git-completion.bash
    . /usr/share/git/git-prompt.sh
else
    function __git_ps1() { true; }
fi

# AccelerEyes configuration
if [ -r ~/.accelereyes ]; then . ~/.accelereyes; fi

# PS1 configuration
if [ -r ~/.ps1 ]; then . ~/.ps1; fi
