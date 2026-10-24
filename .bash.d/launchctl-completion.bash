#!/usr/local/bin/bash
# Bash completion support for launchctl

# TODO:
#  delegate submit -p argument completion to the override executable (argv0 may differ)
#  find way to list: xpc-tokens, xsc-names, port-names
#  complete plist files with relative paths when pwd is one of /library/ ~/library or a launch* dir in those

__launchctl_cmp_log () {
    if [ -n "$DEBUG_LAUNCHCTL_COMP" ]; then
        echo "$*" >> /tmp/launchctl-completion-debug.log
    fi
}

__launchctl_paths ()
{
    readarray -t raw_files < <(\ls -1d {~,}/Library/Launch*/*)
    printf '%s\n' "${raw_files[@]/#$HOME/\~}"
}

__launchctl_array_contains ()
{
    local -n array=$1
    local element=$2
    for el in "${array[@]}"; do
        if [ "$el" = "$element" ]; then
            return 0
        fi
    done
    return 1
}

__launchctl_list_segments_and_sections ()
{
    tr : ' ' <<< "$PATH" | 2>/dev/null xargs fd . --type x | xargs segment_dumper 2>/dev/null | sort -u
}

__launchctl_list_umasks ()
{
    # shellcheck disable=SC2155
    local inner="$(seq 0 7 | paste -sd , -)"
    eval echo "{$inner}{$inner}{$inner}"
}

__launchctl_list_pids ()
{
    command ps axo pid= | tr -d ' ' | sort
}

__launchctl_list_uids ()
{
    dscl . -list /Users UniqueID 2>/dev/null | cut -wf 2 | sort
}

__launchctl_list_asids ()
{
    local uid

    while IFS= read -r uid; do
        launchctl print "user/${uid}" 2>/dev/null |
            awk '$1 == "asid" && $2 == "=" && $3 ~ /^[0-9]+$/ {
                print $3
                exit
            }'
    done < <(__launchctl_list_uids) | sort -un
}

__launchctl_list_domains ()
{
    echo {system,user,login,gui,pid} | tr ' ' '\n'
}

__launchctl_list_domain_targets ()
{
    case "${1:0:1}" in
        s) echo system; return;;
        p) __launchctl_list_pids  | xargs -n1 printf 'pid/%d\n';   return;;
        g) __launchctl_list_uids  | xargs -n1 printf 'gui/%d\n';   return;;
        l) __launchctl_list_asids | xargs -n1 printf 'login/%d\n'; return;;
        u) __launchctl_list_uids  | xargs -n1 printf 'user/%d\n';  return;;
    esac
}

__launchctl_list_service_targets ()
{
    {
        case $2 in
            started) __launchctl_list_started "$1";;
            stopped) __launchctl_list_stopped "$1";;
            all)     __launchctl_list_labels  "$1";;
        esac
    }  | xargs -n1 printf "${1%/}/%s\n" | grep -e "^$1/"
}

__launchctl_list_labels ()
{
    __launchctl_cmp_log "labels: $1"
    launchctl print "${1:-gui/$UID}" | awk '/^\tservices = {/,/}/{print $NF}' | grep -vxFe '{' -e '}'
}

__launchctl_list_started ()
{
    __launchctl_cmp_log "started: $1"
    launchctl print "${1:-gui/$UID}" | awk '/^\tservices = {/,/}/' | awk '$1 != 0{print $NF}' | grep -vxFe '{' -e '}'
}

__launchctl_list_stopped ()
{
    __launchctl_cmp_log "stopped: $1"
    launchctl print "${1:-gui/$UID}" | awk '/^\tservices = {/,/}/' | awk '$1 == 0{print $NF}' | grep -vxFe '{' -e '}'
}

__launchctl_list_envvars ()
{
    printenv | cut -d= -f1 | sort -u
}

# Complete paths from the filesystem or launchctl's common launch-path list.
# Expand ~/ only while looking up candidates, then restore it in the displayed
# replies. The optional source selects the candidate set:
#   filesystem                 Filesystem paths (default)
#   commands                   Commands and filesystem paths
#   launch-items               Common launch paths plus filesystem paths
#   launch-item-directories    Directories containing common launch items
#   launch-library-directories ~/Library and /Library when present (optional $3 suffix)
__launchctl_complete_path ()
{
    local cur=${1-} source=${2:-filesystem} suffix=${3-}
    local expanded_cur=${cur} path reply directory existing duplicate
    local -a replies=() raw_replies=() paths=() candidates=() expanded_candidates=()

    # shellcheck disable=SC2088
    if [[ ${cur} == '~/'* ]]; then
        expanded_cur="${HOME}/${cur#\~/}"
    elif [[ ${cur} == '~' ]]; then
        expanded_cur=${HOME}
    fi

    case ${source} in
        filesystem)
            compgen -o filenames -V replies -f -- "${expanded_cur}"
            ;;
        commands)
            compgen -V replies -cf -- "${expanded_cur}"
            ;;
        launch-items)
            readarray -t paths < <(__launchctl_paths)
            for path in "${paths[@]}"; do
                # shellcheck disable=SC2088
                if [[ ${path} == '~/'* ]]; then
                    expanded_candidates+=("${HOME}/${path#\~/}")
                else
                    expanded_candidates+=("${path}")
                fi
            done
            compgen -V replies -W "${expanded_candidates[*]}" -- "${expanded_cur}"
            compgen -o filenames -V raw_replies -f -- "${expanded_cur}"
            replies+=("${raw_replies[@]}")
            ;;
        launch-item-directories)
            readarray -t paths < <(__launchctl_paths)
            for path in "${paths[@]}"; do
                [[ ${path} == */* ]] || continue
                directory=${path%/*}
                # shellcheck disable=SC2088
                [[ ${directory} == '~/'* ]] && directory="${HOME}/${directory#\~/}"
                candidates+=("${directory}/")
            done
            if ((${#candidates[@]})); then
                readarray -t candidates < <(printf '%s\n' "${candidates[@]}" | sort -u)
                compgen -V replies -W "${candidates[*]}" -- "${expanded_cur}"
            fi
            ;;
        launch-library-directories)
            readarray -t paths < <(__launchctl_paths)
            for path in "${paths[@]}"; do
                # shellcheck disable=SC2088
                case ${path} in
                    '~'/Library/Launch*/*) candidates+=("${HOME}/Library${suffix}");;
                    /Library/Launch*/*)   candidates+=("/Library${suffix}");;
                esac
            done
            if ((${#candidates[@]})); then
                readarray -t candidates < <(printf '%s\n' "${candidates[@]}" | sort -u)
                compgen -V replies -W "${candidates[*]}" -- "${expanded_cur}"
            fi
            ;;
        *)
            compgen -o filenames -V replies -f -- "${expanded_cur}"
            ;;
    esac

    COMPREPLY=()
    for reply in "${replies[@]}"; do
        if [[ ${reply} == "${HOME}" ]]; then
            reply='~'
        elif [[ ${reply} == "${HOME}/"* ]]; then
            # shellcheck disable=SC2088
            reply="~/${reply#"${HOME}/"}"
        fi
        duplicate=0
        for existing in "${COMPREPLY[@]}"; do
            if [[ ${existing} == "${reply}" ]]; then
                duplicate=1
                break
            fi
        done
        ((duplicate)) || COMPREPLY+=("${reply}")
    done
}


# Complete a command nested inside a launchctl subcommand. OFFSET is the index
# of the nested command in the original COMP_WORDS array.
# Prefer bash-completion's mature dispatcher when it is available; the fallback
# below handles function-based compspecs and ordinary compgen-based specs.
__launchctl_complete_wrapped_command ()
{
    local offset=${1:?missing command offset}

    if declare -F _comp_command_offset >/dev/null 2>&1; then
        # _comp_command_offset uses `words`/`cword` when those variables exist.
        # Unset `words` locally to make it use COMP_WORDS, as _comp_command does.
        local words
        unset -v words
        _comp_command_offset "$offset"
        return
    elif declare -F _command_offset >/dev/null 2>&1; then
        _command_offset "$offset"
        return
    fi

    # Fallback for shells without bash-completion's command-offset helper.
    # Copy before introducing local COMP_* variables for compatibility with
    # older Bash releases where local array self-initialization is unreliable.
    local -a original_words=("${COMP_WORDS[@]}")
    local original_line=${COMP_LINE-}
    local original_point=${COMP_POINT:-${#COMP_LINE}}
    local original_cword=${COMP_CWORD}
    local COMP_LINE=${original_line}
    local COMP_POINT=${original_point}
    local COMP_CWORD=$((original_cword - offset))
    local -a COMP_WORDS=("${original_words[@]:offset}")
    local i j tail cur cmd compcmd cspec func spec option_scan option

    # Remove the wrapper words from the textual line and adjust the cursor.
    for ((i = 0; i < offset; i++)); do
        for ((j = 0; j <= ${#COMP_LINE}; j++)); do
            [[ ${COMP_LINE} == "${original_words[i]}"* ]] && break
            COMP_LINE=${COMP_LINE:1}
            ((COMP_POINT--))
        done
        COMP_LINE=${COMP_LINE#"${original_words[i]}"}
        ((COMP_POINT -= ${#original_words[i]}))
    done
    # The word following the wrapper is preceded by one separator. Remove it
    # so the nested command sees a normal command line.
    while [[ ${COMP_LINE} == ' '* ]]; do
        COMP_LINE=${COMP_LINE:1}
        ((COMP_POINT--))
    done

    cur=${COMP_WORDS[COMP_CWORD]-}
    COMPREPLY=()
    if ((COMP_CWORD == 0)); then
        __launchctl_complete_path "${cur}" commands
        return
    fi

    cmd=${COMP_WORDS[0]}
    compcmd=${cmd}
    cspec=$(complete -p -- "${cmd}" 2>/dev/null)
    if [[ -z ${cspec} && ${cmd} == */* ]]; then
        compcmd=${cmd##*/}
        cspec=$(complete -p -- "${compcmd}" 2>/dev/null)
    fi
    if [[ -z ${cspec} ]]; then
        compcmd=${cmd##*/}
        if declare -F _completion_loader >/dev/null 2>&1; then
            _completion_loader "${compcmd}" >/dev/null 2>&1 || :
        elif declare -F _comp_load >/dev/null 2>&1; then
            _comp_load -D -- "${compcmd}" >/dev/null 2>&1 || :
        fi
        cspec=$(complete -p -- "${compcmd}" 2>/dev/null)
    fi

    # Preserve options such as nospace and filenames from the wrapped command's
    # compspec. Ignore compopt errors when running outside an active readline call.
    option_scan=${cspec}
    while [[ ${option_scan} == *' -o '* ]]; do
        option_scan=${option_scan#* -o }
        option=${option_scan%% *}
        compopt -o "${option}" 2>/dev/null || :
        option_scan=${option_scan#"${option}"}
    done

    if [[ ${cspec} == *' -F '* ]]; then
        func=${cspec#* -F }
        func=${func%% *}
        if declare -F "${func}" >/dev/null 2>&1; then
            "${func}" "${cmd}" "${cur}" "${COMP_WORDS[COMP_CWORD-1]-}"
            return
        fi
    elif [[ -n ${cspec} && ${cspec} != *' -C '* ]]; then
        # `complete -p` emits a shell-quoted compspec created by Bash itself.
        # Reuse its compgen options for -W/-A/-G/-f-style completions.
        spec=${cspec#complete}
        spec=${spec% "${compcmd}"}
        if [[ ${spec} != "${cspec#complete}" ]]; then
            eval "compgen -V COMPREPLY ${spec} -- \"\$cur\"" 2>/dev/null || :
            return
        fi
    fi

    # No usable compspec was found. Retain useful defaults rather than returning
    # no candidates: command/file completion for the command's own arguments.
    __launchctl_complete_path "${cur}" filesystem
}


_launchctl2 ()
{
    compopt +o default
    COMPREPLY=()
    local cur="${COMP_WORDS[COMP_CWORD]}"

    # Subcommand list
    local subcommands=(asuser attach blame bootout bootstrap bsexec config debug disable enable error examine getenv help hostinfo kickstart kill limit list load managername managerpid manageruid plist print print-cache print-disabled procinfo reboot remove resolveport setenv start stop submit unload unsetenv variant version bootshell print-token dump-xsc dumpjpcategory dumpstate)

    if [[ ${COMP_CWORD} -eq 1 ]]; then
        compgen -V COMPREPLY -W "${subcommands[*]}" -- "${cur}"
        return
    fi

    __launchctl_cmp_log "${COMP_WORDS[1]} ${COMP_CWORD}"
    case "${COMP_WORDS[1]}" in
        asuser)
            # asuser UID command [args]
            if [[ ${COMP_CWORD} -le 2 ]]; then
                local possibilities=("$(__launchctl_list_uids | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                # The command starts after the UID; delegate its arguments.
                __launchctl_complete_wrapped_command 3
            fi
            return
            ;;
        attach)
            # attach [-ksx] service-target
            local flag_possibilities=(-k -s -x)
            local possibilities=()

            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains COMP_WORDS "$value"; then
                    possibilities+=("$value")
                fi
            done

            if __launchctl_array_contains flag_possibilities "${cur}"; then
                possibilities+=("${cur}")
            fi

            if [ "${cur:0:1}" = '-' ]; then
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [ -z "${cur}" ]; then
                possibilities=("$(__launchctl_list_domains | xargs -n1 printf '%s/\n' | paste -sd ' ' -)")
                compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                possibilities=("$(__launchctl_list_service_targets "${cur%/*}" started | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi

            return
            ;;
        blame)
            # blame service-target
            local possibilities=()
            if [ -z "${cur}" ]; then
                possibilities=("$(__launchctl_list_domains | xargs -n1 printf '%s/\n' | paste -sd ' ' -)")
                compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                possibilities=("$(__launchctl_list_service_targets "${cur%/*}" started | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi
            return
            ;;
        bootout)
            # bootout [--wait] domain-target [service-path service-path2 ...] | service-target
            # --wait is optional and applies only to a single service target.
            # Domain-target form may be followed by one or more service paths.

            local target_index=2
            local -a possibilities=()

            # Once --wait is a completed word, the target moves to word 3.
            if [[ ${COMP_WORDS[2]} == --wait && ${COMP_CWORD} -ge 3 ]]; then
                target_index=3
            fi

            if [[ ${COMP_CWORD} -eq 2 && ${cur} == -* ]]; then
                compgen -V COMPREPLY -W "--wait" -- "${cur}"
            elif [[ ${COMP_CWORD} -eq ${target_index} ]]; then
                if [[ -z ${cur} ]]; then
                    # These are prefixes for the non-system domains, not complete
                    # domain targets. Actual targets returned below have no slash.
                    if [[ ${target_index} -eq 2 ]]; then
                        possibilities=(--wait system user/ login/ gui/ pid/)
                    else
                        possibilities=(system user/ login/ gui/ pid/)
                    fi
                    compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                elif [[ ${cur} == system/* || ${cur} == */*/* ]]; then
                    # A service-target has a service component after its domain.
                    # bootout only operates on started services here.
                    readarray -t possibilities < <(__launchctl_list_service_targets "${cur%/*}" started)
                    compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                else
                    # Match domain targets. At an exact domain target, offer both
                    # forms: the domain itself (with a literal trailing space) and
                    # the same target followed by '/', which starts a service target.
                    # Disable Readline's automatic space so it does not add a space
                    # after the slash candidate or duplicate the explicit space.
                    readarray -t possibilities < <(__launchctl_list_domain_targets "${cur}")
                    local -a matching_domains=() domain_completions=()
                    local possibility
                    compgen -V matching_domains -W "${possibilities[*]}" -- "${cur}"
                    for possibility in "${matching_domains[@]}"; do
                        if [[ ${possibility} == "${cur}" ]]; then
                            domain_completions+=("${possibility} " "${possibility}/")
                        else
                            domain_completions+=("${possibility}")
                        fi
                    done
                    compopt -o nospace
                    COMPREPLY=("${domain_completions[@]}")
                fi
            else
                local target="${COMP_WORDS[target_index]}"

                # The service-target form is a single target, not a domain followed
                # by service paths. Only domain-target form accepts path arguments.
                if [[ ${target} == system/* || ${target} == */*/* ]]; then
                    return
                fi

                if [[ -z ${cur} ]]; then
                    compopt -o nospace 2>/dev/null || :
                    __launchctl_complete_path "${cur}" launch-item-directories
                else
                    __launchctl_complete_path "${cur}" launch-items
                fi
            fi
            return
            ;;
        bootstrap)
            # bootstrap domain-target [service-path service-path2 ...] | service-target
            local possibilities=()
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                if [ -z "${cur}" ]; then
                    possibilities=("$(__launchctl_list_domains | xargs -n1 printf '%s/\n' | paste -sd ' ' -)")
                    compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                else
                    possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                    compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                fi
            else
                local prev="${COMP_WORDS[2]}"
                if [[ -z ${cur} ]]; then
                    compopt -o nospace 2>/dev/null || :
                    __launchctl_complete_path "${cur}" launch-library-directories /
                    COMPREPLY+=("${prev}/")
                elif [[ ${cur} == "${prev}/"* ]]; then
                    readarray -t possibilities < <(__launchctl_list_service_targets "${cur%/*}" stopped)
                    compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                else
                    __launchctl_complete_path "${cur}" launch-items
                fi
            fi
            return
            ;;
        bsexec)
            # bsexec PID command [args]
            if [[ ${COMP_CWORD} -le 2 ]]; then
                local possibilities=("$(__launchctl_list_pids | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                # The command starts after the PID; delegate its arguments.
                __launchctl_complete_wrapped_command 3
            fi
            return
            ;;
        config)
            # config system | user parameter value
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                compgen -V COMPREPLY -W "system user" -- "${cur}"
            elif [[ ${COMP_CWORD} -eq 3 ]]; then
                compgen -V COMPREPLY -W "umask path" -- "${cur}"
            else
                case "${COMP_WORDS[3]}" in
                    umask) compgen -V COMPREPLY -W "$(__launchctl_list_umasks)" -- "${cur}";;
                    path) compopt -o nospace 2>/dev/null || :; __launchctl_complete_path "${cur}" filesystem;;
                esac
            fi
            return
            ;;
        debug)
            # debug service-target [--program <program-path>] [--guard-malloc] [--malloc-stack-logging] [--malloc-nano-allocator] [--start-suspended] [--debug-libraries] [--introspection-libraries] [--NSZombie] [--32] [--stdin [stdin-path]] [--stdout [stdout-path]] [--stderr [stderr-path]] [--environment VARIABLE=value ...] [--] [argv0 argv1 ...]
            # Keep this list aligned with the options in the command synopsis.
            local -a flag_possibilities=(
                --program --guard-malloc --malloc-stack-logging
                --malloc-nano-allocator --start-suspended --debug-libraries
                --introspection-libraries --NSZombie --32 --stdin --stdout
                --stderr --environment --
            )
            local -a possibilities=() flag_candidates=() env_names=() path_candidates=()
            local -a used_flags=()
            local value prev='' in_argv=0 in_environment=0
            local expect_program_value=0 expect_optional_path=0 prev_is_value=0
            local i

            # Parse only words before the cursor. A current word equal to `--`
            # is still an option candidate, not the delimiter. Skip the value
            # of --program so a path that happens to look like a flag is not
            # mistaken for an option already supplied.
            for ((i = 3; i < COMP_CWORD; i++)); do
                value=${COMP_WORDS[i]}
                if (( expect_program_value )); then
                    expect_program_value=0
                    if (( i == COMP_CWORD - 1 )); then prev_is_value=1; fi
                    continue
                fi
                if (( expect_optional_path )); then
                    expect_optional_path=0
                    if [[ ${value} != -* ]]; then
                        if (( i == COMP_CWORD - 1 )); then prev_is_value=1; fi
                        continue
                    fi
                fi
                if [[ ${value} == -- ]]; then
                    in_argv=1
                    break
                fi
                case ${value} in
                    --program)
                        used_flags+=("${value}")
                        in_environment=0
                        expect_program_value=1
                        ;;
                    --stdin|--stdout|--stderr)
                        used_flags+=("${value}")
                        in_environment=0
                        expect_optional_path=1
                        ;;
                    --environment)
                        used_flags+=("${value}")
                        in_environment=1
                        ;;
                    --*)
                        used_flags+=("${value}")
                        in_environment=0
                        ;;
                esac
            done

            # The first argument must be a service-target.
            if (( COMP_CWORD == 2 )); then
                if [[ -z ${cur} ]]; then
                    readarray -t possibilities < <(__launchctl_list_domains)
                    compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                elif [[ ${cur} != */*/* && ${cur} != system/* ]]; then
                    readarray -t possibilities < <(__launchctl_list_domain_targets "${cur}")
                    compopt -o nospace 2>/dev/null || :
                    compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                else
                    readarray -t possibilities < <(__launchctl_list_service_targets "${cur%/*}" all)
                    compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                fi
                return
            fi

            # Completion after the service-target: options, option values, or argv.
            if (( in_argv )); then
                __launchctl_complete_path "${cur}" commands
                return
            fi

            prev=${COMP_WORDS[COMP_CWORD-1]}

            # --program requires a path. It is not an optional argument like
            # --stdin/--stdout/--stderr, so only filesystem candidates belong here.
            if [[ ${prev} == --program && ${prev_is_value} -eq 0 ]]; then
                __launchctl_complete_path "${cur}"
                return
            fi

            # --environment accepts one or more VARIABLE=value words. Keep
            # completing variable names until another option or `--` is entered.
            if (( in_environment )) && [[ ${cur} != -* ]]; then
                if [[ ${cur} != *=* ]]; then
                    readarray -t env_names < <(__launchctl_list_envvars)
                    compopt -o nospace 2>/dev/null || :
                    compgen -o nospace -S = -V COMPREPLY -W "${env_names[*]}" -- "${cur}"
                fi
                return
            fi

            # --stdin/--stdout/--stderr have optional path arguments. For an
            # empty word, offer both possible path entries and the next options;
            # when the user starts a flag, complete flags instead of filenames.
            if [[ ${prev_is_value} -eq 0 && ( ${prev} == --stdin || ${prev} == --stdout || ${prev} == --stderr ) ]]; then
                if [[ ${cur} == -* ]]; then
                    : # fall through to option completion
                elif [[ -z ${cur} ]]; then
                    __launchctl_complete_path "${cur}"
                    path_candidates=("${COMPREPLY[@]}")
                    for value in "${flag_possibilities[@]}"; do
                        if ! __launchctl_array_contains used_flags "${value}"; then
                            flag_candidates+=("${value}")
                        fi
                    done
                    COMPREPLY=("${path_candidates[@]}" "${flag_candidates[@]}")
                    return
                else
                    __launchctl_complete_path "${cur}"
                    return
                fi
            fi

            # Offer only flags not already used. The current word is excluded
            # from used_flags, so an unused exact flag remains a valid candidate.
            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains used_flags "${value}"; then
                    flag_candidates+=("${value}")
                fi
            done
            compgen -V COMPREPLY -W "${flag_candidates[*]}" -- "${cur}"
            return
            ;;
        disable)
            # disable service-target
            if [ -z "${cur}" ]; then
                local possibilities=("$(__launchctl_list_domains | paste -sd ' ' -)")
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                local possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                local possibilities=("$(__launchctl_list_service_targets "${cur%/*}" started | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi
            return
            ;;
        enable)
            # enable service-target
            if [ -z "${cur}" ]; then
                local possibilities=("$(__launchctl_list_domains | paste -sd ' ' -)")
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                local possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                local possibilities=("$(__launchctl_list_service_targets "${cur%/*}" stopped | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi
            return
            ;;
        error)
            # error [posix|mach|bootstrap] code
            local possibilities=()
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                possibilities=(posix mach bootstrap)
            else
                case "${COMP_WORDS[2]}" in
                    bootstrap) possibilities=("$(seq 0 56) $(seq 1000 1003) $(seq 1100 1105) $(seq 1601 1607)");;
                    mach) possibilities=("$(seq 1 56)");;
                    posix) possibilities=("$(seq 0 164)");;
                esac
            fi
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        examine)
            # examine [tool arg0 arg1 @PID ...]
            # The optional tool is a nested command, so use its own completion.
            local original_cword=${COMP_CWORD}
            __launchctl_complete_wrapped_command 2
            # @PID is a launchctl placeholder accepted in the tool's arguments.
            if ((original_cword > 2)) && [[ '@PID' == "${cur}"* ]]; then
                COMPREPLY+=('@PID')
            fi
            return
            ;;
        getenv)
            # getenv key
            local possibilities=("$(__launchctl_list_envvars | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        help)
            # help <subcommand>
            compgen -V COMPREPLY -W "${subcommands[*]}" -- "${cur}"
            return
            ;;
        hostinfo)
            # hostinfo
            return
            ;;
        kickstart)
            # kickstart [-k] [-p] <service-target>
            local flag_possibilities=(-k -p)
            local possibilities=()

            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains COMP_WORDS "$value"; then
                    possibilities+=("$value")
                fi
            done

            if __launchctl_array_contains flag_possibilities "${cur}"; then
                possibilities+=("${cur}")
            fi

            if [ "${cur:0:1}" = '-' ]; then
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [ -z "${cur}" ]; then
                possibilities+=("$(__launchctl_list_domains | xargs -n1 printf '%s/\n' | paste -sd ' ' -)")
                compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                possibilities=("$(__launchctl_list_service_targets "${cur%/*}" started | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi

            return
            ;;
        kill)
            # kill <signal-number|signal-name> <service-target>
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                compgen -V COMPREPLY -A signal -W "$(seq 1 31)" -- "${cur}"
            else
                if [ -z "${cur}" ]; then
                    local possibilities=("$(__launchctl_list_domains | paste -sd ' ' -)")
                    compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                    local possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                    compopt -o nospace #doesn't work if set in compgen here...
                    compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                else
                    local possibilities=("$(__launchctl_list_service_targets "${cur%/*}" stopped | paste -sd ' ' -)")
                    compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                fi
            fi

            return
            ;;
        limit)
            # limit [<limit-name> [<both-limits> | <soft-limit> <hard-limit>]
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                local possibilities=("cpu filesize data stack core rss memlock maxproc maxfiles")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                compgen -V COMPREPLY -W "unlimited" -- "${cur}"
            fi
            return
            ;;
        list)
            # list [service-name]
            local possibilities=("$(__launchctl_list_labels | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        load)
            # load [-wF] [-S sessiontype] [-D searchpath] <service-path, service-path2, ...>
            local flag_possibilities=(-w -F -S -D)
            local possibilities=()
            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains COMP_WORDS "$value"; then
                    possibilities+=("$value")
                fi
            done
            if __launchctl_array_contains flag_possibilities "${cur}"; then
                possibilities+=("${cur}")
            fi

            if [[ ${COMP_CWORD} -gt 2 ]]; then
                case "${COMP_WORDS[COMP_CWORD-1]}" in
                    -S) compgen -V COMPREPLY -W "Aqua Background LoginWindow StandardIO System" -- "${cur}"; return ;;
                    -D) compgen -V COMPREPLY -W "system local network user" -- "${cur}"; return ;;
                esac
            fi

            if [[ -z ${cur} ]]; then
                __launchctl_complete_path "${cur}" launch-library-directories
                COMPREPLY+=("${possibilities[@]}")
            elif [[ ${cur:0:1} == '-' ]]; then
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                __launchctl_complete_path "${cur}" launch-items
            fi
            return
            ;;
        managername)
            # managername
            return
            ;;
        managerpid)
            # managerpid
            return
            ;;
        manageruid)
            # manageruid
            return
            ;;
        plist)
            # plist [segment,section] Mach-O
            if [[ ${COMP_CWORD} -eq 2 ]]; then
                local possibilities=("$(__launchctl_list_segments_and_sections | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                __launchctl_complete_path "${cur}" filesystem
            fi
            return
            ;;
        print)
            # launchctl print <domain-target> | <service-target>
            if [ -z "${cur}" ]; then
                local possibilities=("$(__launchctl_list_domains | paste -sd ' ' -)")
                compgen -o nospace -S / -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]] && [[ "${cur}" != system/* ]]; then
                local possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compopt -o nospace #doesn't work if set in compgen here...
                compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
                if [ ${#COMPREPLY[@]} -lt 2 ]; then
                    COMPREPLY+=("${COMPREPLY[@]}/")
                fi
            else
                local possibilities=("$(__launchctl_list_service_targets "${cur%/*}" all | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi
            return
            ;;
        print-cache)
            # print-cache
            return
            ;;
        print-disabled)
            # print-disabled domain-target
            if [ -z "${cur}" ]; then
                local possibilities=("$(__launchctl_list_domains | xargs -n1 printf '%s/\n' | paste -sd ' ' -)")
                compgen -o nospace -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            elif [[ "${cur}" != */*/* ]]; then
                local possibilities=("$(__launchctl_list_domain_targets "${cur}" | paste -sd ' ' -)")
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            fi
            return
            ;;
        procinfo)
            # procinfo pid
            local possibilities=("$(__launchctl_list_pids | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        reboot)
            # reboot [system|halt|userspace|logout|apps] [-s]
            local possibilities=(system halt userspace logout apps)
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        remove)
            # remove label
            local possibilities=("$(__launchctl_list_labels | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        resolveport)
            # resolveport owner-pid port-name
            local possibilities=("$(__launchctl_list_pids | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        setenv)
            # setenv key value
            return
            ;;
        start)
            # start label
            local possibilities=("$(__launchctl_list_labels | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        stop)
            # stop label
            local possibilities=("$(__launchctl_list_labels | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        submit)
            # submit -l label [-p executable] [-o stdout-path] [-e stderr-path] -- command [arg0] [arg1] [...]
            local flag_possibilities=(-l -p -o -e)
            local separator_index=-1 separator_scan override_program=0

            # Treat -- as a delimiter only after it is a completed word. If
            # there is no -p override, the following command owns its arguments
            # and can use its own Bash completion.
            for ((separator_scan = 2; separator_scan < COMP_CWORD; separator_scan++)); do
                if [[ ${COMP_WORDS[separator_scan]} == -- ]]; then
                    separator_index=${separator_scan}
                    break
                fi
            done
            if ((separator_index >= 0)); then
                for ((separator_scan = 2; separator_scan < separator_index; separator_scan++)); do
                    case ${COMP_WORDS[separator_scan]} in
                        -l|-o|-e) ((separator_scan++));;
                        -p) override_program=1; ((separator_scan++));;
                    esac
                done
                if (( ! override_program )); then
                    __launchctl_complete_wrapped_command "$((separator_index + 1))"
                else
                    # With -p, words after -- are an argv vector for the override
                    # executable, not the name of the executable being run.
                    __launchctl_complete_path "${cur}" commands
                fi
                return
            fi

            if __launchctl_array_contains COMP_WORDS "-l"; then
                flag_possibilities+=(--)
            fi

            local possibilities=()
            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains COMP_WORDS "$value"; then
                    possibilities+=("$value")
                fi
            done

            if __launchctl_array_contains flag_possibilities "${cur}"; then
                possibilities+=("${cur}")
            fi

            case "${COMP_WORDS[COMP_CWORD-1]}" in
                -p) __launchctl_complete_path "${cur}" commands; return;;
                -o) __launchctl_complete_path "${cur}" filesystem; return;;
                -e) __launchctl_complete_path "${cur}" filesystem; return;;
                -l) return;;
                *) compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"; return;;
            esac
            return
            ;;
        unload)
            # unload [-wF] [-S sessiontype] [-D searchpath] paths ...
            local flag_possibilities=(-w -F -S -D)

            local possibilities=()
            for value in "${flag_possibilities[@]}"; do
                if ! __launchctl_array_contains COMP_WORDS "$value"; then
                    possibilities+=("$value")
                fi
            done
            if __launchctl_array_contains flag_possibilities "${cur}"; then
                possibilities+=("${cur}")
            fi
            case "${COMP_WORDS[COMP_CWORD-1]}" in
                -S) compgen -V COMPREPLY -W "Aqua Background LoginWindow StandardIO System" -- "${cur}"; return ;;
                -D) compgen -V COMPREPLY -W "system local network user" -- "${cur}"; return ;;
            esac
            if [[ -z ${cur} ]]; then
                __launchctl_complete_path "${cur}" launch-library-directories
                COMPREPLY+=("${possibilities[@]}")
            elif [[ ${cur:0:1} == '-' ]]; then
                compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            else
                __launchctl_complete_path "${cur}" launch-items
            fi
            return
            ;;
        unsetenv)
            # unsetenv key
            local possibilities=("$(__launchctl_list_envvars | paste -sd ' ' -)")
            compgen -V COMPREPLY -W "${possibilities[*]}" -- "${cur}"
            return
            ;;
        variant)
            # variant
            return
            ;;
        version)
            # version
            return
            ;;
        bootshell)
            # bootshell [continue]
            compgen -V COMPREPLY -W "continue" -- "${cur}"
            return
            ;;
        print-token)
            # print-token <xpc token>
            return
            ;;
        dump-xsc)
            # dump-xsc <name>
            return
            ;;
        dumpjpcategory)
            # dumpjpcategory
            return
            ;;
        dumpstate)
            # dumpstate
            return
            ;;
    esac
}

complete -F _launchctl2 launchctl
