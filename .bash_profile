#!/usr/local/bin/bash

#exit if non-interactive
if [[  ! ( $- =~ "i" ) ]]; then
    return 0
fi
#environment variables
export CATALINA_HOME=/Library/Tomcat/
if /usr/libexec/java_home &>/dev/null; then
    export JAVA_HOME=`/usr/libexec/java_home`
fi
export PASSENGER_INSTANCE_REGISTRY_DIR=/tmp
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS=dxHxgxgxBxfxhxCxGxExFx
export GREP_OPTIONS='--color=auto --line-buffered'
export RSYNC_RSH="ssh -oRequestTTY=no -oRemoteCommand=none"
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
export LESS_TERMCAP_us=$'\E[01;36m'
export RIPGREP_CONFIG_PATH=~/.ripgrep.rc

stty -ixon

#options (-s set, -u unset)
shopt -s histappend
shopt -s extglob
shopt -s xpg_echo
shopt -s globstar
shopt -s nocaseglob
shopt -s nocasematch

#sudo nvram boot-args="mbasd=1" # enable macbook air superdrive

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

#completion
if [ -f ~/.bash.d/completion ]; then
    . ~/.bash.d/completion
fi

#development
if [ -f ~/.bash.d/development ]; then
    . ~/.bash.d/development
fi

#emacs
if [ -f ~/.bash.d/emacs ]; then
    . ~/.bash.d/emacs
fi

if which rbenv > /dev/null; then
    eval "$(rbenv init -)";
fi

if [ "Darwin" = `uname` ]; then
    archey -c -p -o -l
    calendar
fi
