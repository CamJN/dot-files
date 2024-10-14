# no shebang

#exit if non-interactive
if [[  ! ( $- =~ "i" ) ]]; then
    return 0
fi

#exit if dumb terminal
if [ "$TERM" = "dumb" ]; then
    return 0
fi

#environment variables
export DO_NOT_TRACK=1
export PASSENGER_INSTANCE_REGISTRY_DIR=/tmp
export DOCKER_SOCK="unix://$HOME/.colima/default/docker.sock"
export DOCKER_HOST="$DOCKER_SOCK"
export __CF_USER_TEXT_ENCODING="0x1F5:0x8000100:0x52"
export CLICOLOR=1
export LSCOLORS=dxHxgxgxBxfxhxCxGxExFx
export GREP_OPTIONS='--binary-file=without-match --color=auto --line-buffered --exclude=*.xhprof'
export RSYNC_RSH="ssh -oRequestTTY=no -oRemoteCommand=none"
export PAGER="less -R"
export BAT_PAGER="less -R"
export MORE='-R -i'
export LESS="$MORE"
export GZIP="-9"
export PIGZ="$GZIP"
export GZIP_OPT="$GZIP"
export XZ_OPT="$GZIP"
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;37m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;36m'
export RIPGREP_CONFIG_PATH=~/.ripgrep.rc
export SUDO_EDITOR=mg
# export GPG_TTY=$(tty) # for if the Curses based Pinentry does not work

stty -ixon

#options (-s set, -u unset)
shopt -s nullglob
shopt -s extglob
shopt -s xpg_echo
shopt -s globstar
shopt -s nocaseglob
shopt -s nocasematch
shopt -s direxpand

#sudo nvram boot-args="mbasd=1" # enable macbook air superdrive

#paths
if [ -f ~/.bash.d/paths ]; then
# shellcheck source=./.bash.d/paths
    . ~/.bash.d/paths
fi

#prompts
if [ -f ~/.bash.d/prompts ]; then
# shellcheck source=./.bash.d/prompts
    . ~/.bash.d/prompts
fi

#functions
if [ -f ~/.bash.d/functions ]; then
# shellcheck source=./.bash.d/functions
    . ~/.bash.d/functions
fi

#aliases
if [ -f ~/.bash.d/aliases ]; then
# shellcheck source=./.bash.d/aliases
    . ~/.bash.d/aliases
fi

#completion
if [ -f ~/.bash.d/completion ]; then
# shellcheck source=./.bash.d/completion
    . ~/.bash.d/completion
fi

#development
if [ -f ~/.bash.d/development ]; then
# shellcheck source=./.bash.d/development
    . ~/.bash.d/development
fi

#emacs
if [ -f ~/.bash.d/emacs ]; then
# shellcheck source=./.bash.d/emacs
    . ~/.bash.d/emacs
fi

#history
if [ -f ~/.bash.d/history ]; then
# shellcheck source=./.bash.d/history
    . ~/.bash.d/history
fi

if [ "Darwin" = "$(uname)" ] && command -v neowofetch >/dev/null; then
    neowofetch
fi
