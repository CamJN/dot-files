#!/usr/local/bin/bash
#
# Bash completion support for launchctl
__launchctl_list_sigs ()
{
    # shellcheck disable=SC2046
    echo {,SIG}{HUP,INT,QUIT,ILL,TRAP,ABRT,EMT,FPE,KILL,BUS,SEGV,SYS,PIPE,ALRM,TERM,URG,STOP,TSTP,CONT,CHLD,TTIN,TTOU,IO,XCPU,XFSZ,VTALRM,PROF,WINCH,INFO,USR1,USR2} $(seq 1 31)
}

__launchctl_list_pids ()
{
    command ps axo pid=
}

__launchctl_list_uids ()
{
    dscl . -list /Users UniqueID 2>/dev/null | awk '{print $2}'
}

join_list ()
{
    tr -d ' ' | tr '\n' ',' | sed -e 's/,$//'
}

__launchctl_list_domains ()
{
    echo {system,user,gui,login,session,pid}/
}

__launchctl_list_subdomains ()
{
    pids=$(__launchctl_list_pids | sed -Ee 's@^ *(.*)@pid/\1/@g'       | tr '\n' ' ')
    uids=$(__launchctl_list_uids | sed -Ee 's@(.*)@user/\1/ gui/\1/@g' | tr '\n' ' ')
    #asids=  {login,session}/{asids...}
    echo "system/ ${uids} ${pids}"
}

__launchctl_list_service_targets ()
{
    join -j 99 -t '' <(__launchctl_list_subdomains | tr ' ' '\n')  <(__launchctl_list_labels) | tr '\n' ' '
}

__launchctl_list_labels ()
{
    launchctl list | awk 'NR>1 && $3 !~ /0x[0-9a-fA-F]+\.(anonymous|mach_init)/ {print $3}'
}

__launchctl_list_started ()
{
    launchctl list | awk 'NR>1 && $3 !~ /0x[0-9a-fA-F]+\.(anonymous|mach_init)/ && $1 !~ /-/ {print $3}'
}

__launchctl_list_stopped ()
{
    launchctl list | awk 'NR>1 && $3 !~ /0x[0-9a-fA-F]+\.(anonymous|mach_init)/ && $1 ~ /-/ {print $3}'
}
_launchctl ()
{
    compopt +o default
    COMPREPLY=()
    local cur="${COMP_WORDS[COMP_CWORD]}"
    local prev="${COMP_WORDS[COMP_CWORD-1]}"
    local two_prev="${COMP_WORDS[COMP_CWORD-2]}"

    # Subcommand list
    local subcommands="bootstrap bootout enable disable uncache kickstart attach debug kill blame print print-cache print-disabledplist procinfo hostinfo resolveport limit runstats examine config dumpstate reboot bootshell load unload remove list start stop setenv unsetenv getenv bsexec asuser submit managerpid manageruid managername error variant version help"
    [[ ${COMP_CWORD} -eq 1 ]] && {
        mapfile -t COMPREPLY < <(compgen -W "${subcommands}" -- "${cur}")
        return
    }
    if [[ ${COMP_CWORD} -eq 2 ]]; then
        case "$prev" in
            print|enable|disable|blame|runstats|bootstrap|bootout|debug)
                compopt -o nospace
                if [[ "${cur}" == */*/* ]]; then
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_service_targets)" -- "${cur}")
                elif [[ "${cur}" == */* ]]; then
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_subdomains)" -- "${cur}")
                else
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_domains)" -- "${cur}")
                fi
                return
                ;;
            submit)
                mapfile -t COMPREPLY < <(compgen -W "-l" -- "${cur}")
                return
                ;;
            help)
                mapfile -t COMPREPLY < <(compgen -W "$subcommands" -- "${cur}")
                return
                ;;
            kickstart)
                mapfile -t COMPREPLY < <(compgen -W "-k -p -s" -- "${cur}")
                return
                ;;
            reboot)
                mapfile -t COMPREPLY < <(compgen -W "-s system halt userspace reroot logout apps" -- "${cur}")
                return
                ;;
            attach)
                mapfile -t COMPREPLY < <(compgen -W "-k -s -x" -- "${cur}")
                return
                ;;
            bootshell)
                mapfile -t COMPREPLY < <(compgen -W "continue" -- "${cur}")
                return
                ;;
            procinfo|resolveport)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_pids)" -- "${cur}")
                return
                ;;
            error)
                mapfile -t COMPREPLY < <(compgen -W "posix mach bootstrap" -- "${cur}")
                return
                ;;
            remove|list|uncache)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_labels)" -- "${cur}")
                return
                ;;
            start)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_stopped)" -- "${cur}")
                return
                ;;
            stop)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_started)" -- "${cur}")
                return
                ;;
            kill)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_sigs)" -- "${cur}")
                return
                ;;
            bsexec)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_pids)" -- "${cur}")
                return
                ;;
            asuser)
                mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_uids)" -- "${cur}")
                return
                ;;
            config)
                mapfile -t COMPREPLY < <(compgen -W 'system user' -- "${cur}")
                return
                ;;
            load|unload)
                compopt -o filenames
                compopt -o nospace
                mapfile -t COMPREPLY < <(compgen -f -- "${cur}")
                return
                ;;
        esac
    fi
    if [[ ${COMP_CWORD} -eq 3 ]]; then
        case "$two_prev" in
            config)
                mapfile -t COMPREPLY < <(compgen -W "umask path" -- "${cur}")
                return
                ;;
            reboot)
                mapfile -t COMPREPLY < <(compgen -W "system halt userspace reroot logout apps" -- "${cur}")
                return
                ;;
            kill|attach|kickstart)
                compopt -o nospace
                if [[ "${cur}" == */*/* ]]; then
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_service_targets)" -- "${cur}")
                elif [[ "${cur}" == */* ]]; then
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_subdomains)" -- "${cur}")
                else
                    mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_domains)" -- "${cur}")
                fi
                return
                ;;
            bootstrap|bootout)
                compopt -o filenames
                compopt -o nospace
                mapfile -t COMPREPLY < <(compgen -f -- "${cur}")
                return
                ;;
            # resolveport)
            #     mapfile -t COMPREPLY < <(compgen -W "$(__launchctl_list_ports)" -- "${cur}")
            #     return
            #     ;;
        esac
    fi
}

complete -F _launchctl launchctl
