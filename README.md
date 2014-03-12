conf
=========

This repository contains semi-minimalist linux configuration files

Contents
----------

- `bashrc`:
    - default editor is `emacs`
    - binds up and down array keys to history search
- `ps1`:
    - formatting for prompt: `hostname:dir.gitbranch $`
- `emacs`:
    - Adds plugins for `markdown-mode`, `go-mode`, `ESS` and `emacs-jabber`
    - `C`, `C++` and `Java` use BSD style braces
    - tab is 4 spaces
    - shows trailing whitespaces in `C`, `C++`, `Java`, `fortran` and `go`
    - `\C-c \C-c` mapped to compile
    - `\C-cc` mapped to comment region
- `gitconfig`
    - pretty formatting and aliases for git
    - Please change `user.name` and `user.email`
- `screenrc`
    - Opens `emacs`, and `htop` by default
- `vimrc`
    - Work in progress

- `config`
    - Creates backups of current config files and symlinks from the repo

- `emacs_plugins`
    - Stand alone script that downloads and installs plugins necessary for `emacs`
    - Run automatically from `config`. Need not run seperately

LICENSE
-----------
MIT License
