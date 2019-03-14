#!/bin/bash

if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

if [[ $- =~ "i" ]]; then
    if [ -n $INSIDE_EMACS ]; then
        if [ -f ~/.bash_profile ]; then
            . ~/.bash_profile
        fi
    fi
fi
