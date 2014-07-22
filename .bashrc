#!/bin/bash

if [ -n $INSIDE_EMACS ]; then

    if [ -f ~/.bash_profile ]; then
        . ~/.bash_profile
    fi

fi
