# no shebang

if [ -x /usr/libexec/path_helper ]; then
    # shellcheck disable=SC1090
    source <(/usr/libexec/path_helper -s)
fi

if [[ $- =~ "i" ]]; then
    if [ -z "${INSIDE_EMACS-}" ]; then
        if [ -f ~/.bash_profile ]; then
            # shellcheck source=./.bash_profile
            source ~/.bash_profile
        fi
        if [ -n "${SSH_AUTH_SOCK-}" ]; then
            # shellcheck disable=SC2263
            echo "export SSH_AUTH_SOCK='$(launchctl getenv SSH_AUTH_SOCK)'"
        fi
    fi
fi
