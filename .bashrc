# Check for an interactive sokession
[ -z "$PS1" ] && return

# General
alias ls='ls --color=auto'
alias ll='ls -al'
alias mdb='matlab -Dgdb'
alias m='matlab -nodesktop -nosplash'
alias e='emacs -nw'
alias E='sudo emacs -nw'
alias grep='grep --color'

# Arch Linux specific aliases
if [ `grep Arch /etc/issue | wc -l` -ge 0 ]; then
    alias pacman='sudo pacman-color'
fi

calc() { echo "$@" | bc -l; }
note() { echo "$@" >> ~/Documents/notes; }

# Editors
export EDITOR='emacs -nw'
export SVN_EDITOR=EDITOR

# Paths
export CUDADIR=/usr/local/cuda
export BINS=$CUDADIR/bin
export PATH=$PATH:$BINS

# GIT PS1 and auto complete
if [ -r /etc/bash_completion.d/git ]; then
    . /etc/bash_completion.d/git
else
    function __git_ps1() { true; }
fi

# AccelerEyes configuration
if [ -r .accelereyes ]; then . .accelereyes; fi

# PS1 configuration
if [ -r .ps1 ]; then . .ps1; fi