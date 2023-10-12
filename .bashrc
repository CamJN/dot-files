# no shebang

if [ -x /usr/libexec/path_helper ]; then
    eval `/usr/libexec/path_helper -s`
fi

if [[ $- =~ "i" ]]; then
    if [ -n $INSIDE_EMACS ]; then
        if [ -f ~/.bash_profile ]; then
            . ~/.bash_profile
        fi
        if [ -z "$SSH_AUTH_SOCK" ]; then
            echo export SSH_AUTH_SOCK=/tmp/com.apple.launchd.*/Listeners
            # export SSH_AUTH_SOCK=$(launchctl getenv SSH_AUTH_SOCK)
        fi
    fi
fi
