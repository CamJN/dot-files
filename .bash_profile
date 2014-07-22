#!/bin/bash

#environment variables
export CATALINA_HOME=/Library/Tomcat/
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=dxHxgxgxBxfxhxCxGxExFx
export GREP_OPTIONS='-I --color=always --exclude=*.xhprof'
export HISTFILE=~/.bash.d/history
export HISTSIZE=1000000
export HISTFILESIZE=1000000
export MORE='-R -i'
export LESS="$MORE"
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;37m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'

stty -ixon

#options
shopt -s histappend
shopt -s extglob
shopt -s xpg_echo

bind 'set completion-ignore-case on' # only works in interactive mode, so bashrc is better i think.
#bind 'set skip-completed-text on' # requires bash 4

#completion
if [ -f ~/.bash.d/completion ]; then
    . ~/.bash.d/completion
fi

#paths
if [ -f ~/.bash.d/paths ]; then
    . ~/.bash.d/paths
fi

#prompts
if [ -f ~/.bash.d/prompts ]; then
    . ~/.bash.d/prompts
fi

#functions
if [ -f ~/.bash.d/functions ]; then
    . ~/.bash.d/functions
fi

#aliases
if [ -f ~/.bash.d/aliases ]; then
    . ~/.bash.d/aliases
fi

#development
if [ -f ~/.bash.d/development ]; then
    . ~/.bash.d/development
fi

#emacs
if [ -f ~/.bash.d/emacs ]; then
    . ~/.bash.d/emacs
fi

if [ "Darwin" = `uname` ]; then
    calendar
fi
if which rbenv > /dev/null; then
    eval "$(rbenv init -)";
fi
