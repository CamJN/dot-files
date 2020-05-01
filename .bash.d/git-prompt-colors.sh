override_git_prompt_colors() {
    GIT_PROMPT_THEME_NAME="Custom"

    PathShort="\w";

    EndColor="${ResetColor}"
    ResetColor=""
    StatusRed="[(01;(38;05;196))m"

    function git_prompt_command_ok {
        StatusGreen="[(01;(38;05;22))m"
        StatusAmber="[(01;(38;05;214))m"
        if $_isroot; then
            echo ${StatusAmber};
        else
            echo ${StatusGreen};
        fi
    }

    # indicator if the last command returned with an exit code of 0
    GIT_PROMPT_COMMAND_OK="$(git_prompt_command_ok)"
    # indicator if the last command returned with an exit code of other than 0
    GIT_PROMPT_COMMAND_FAIL="${StatusRed}"

    LAST_COMMAND_INDICATOR='\[${LAST_COMMAND_INDICATOR}\]' # magic
    GIT_PROMPT_START_USER="${LAST_COMMAND_INDICATOR}\u@\h:${PathShort}${EndColor}"
    GIT_PROMPT_START_ROOT="${GIT_PROMPT_START_USER}"
    GIT_PROMPT_END_USER="${White} \\$ ${EndColor}"
    GIT_PROMPT_END_ROOT="${GIT_PROMPT_END_USER}"
}

reload_git_prompt_colors "Custom"
