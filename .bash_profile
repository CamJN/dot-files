# no shebang

#exit if non-interactive
if [[  ! ( $- =~ "i" ) ]]; then
    return 0
fi

#exit if dumb terminal
if [ "$TERM" = "dumb" ]; then
    return 0
fi

#env
if [ -f ~/.bash.d/env ]; then
# shellcheck source=./.bash.d/env
    . ~/.bash.d/env
fi

#options
if [ -f ~/.bash.d/options ]; then
# shellcheck source=./.bash.d/options
    . ~/.bash.d/options
fi

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
