#!/usr/local/bin/bash

function fplay {
    TYPEREGEX='.*\.(3g[2|p]|a(ac|c3|dts|if[c|f]?|mr|nd|u)|caf|m4[a|r]|mp([1-4|a]|eg[0,9]?)|sd2|wav)'
    DOAPPLESCRIPT=0
    APPLESCRIPTPART1='tell application "QuickTime Player" to delay 1'
    APPLESCRIPTPART2='tell application "QuickTime Player" to play the first document'
    MUSICROOT=~/Music/
    OUTPUT1="/dev/null"
    OUTPUT2="/dev/null"
    AUTHOR=""
    ALBUM=""
    NAME=""
    PLAY=1
    INFO=0
    options_found=0
    while getopts ":sd:hgia:A:o:v" OPTION
    do
        options_found=1
        # shellcheck disable=SC2214
        case $OPTION in
            d|+d)
                MUSICROOT="$OPTARG"
                ;;
            g|+g)
                DOAPPLESCRIPT=1
                ;;
            s|+s)
                PLAY=0
                ;;
            i|+i)
                INFO=1
                ;;
            a|+a)
                AUTHOR="AND author:$OPTARG"
                ;;
            A|+A)
                ALBUM="AND album:$OPTARG"
                ;;
            o|+o)
                OUTPUT2="$OPTARG"
                ;;
            v|+v)
                OUTPUT1="/dev/stdout"
                ;;
            h|+h)
                echo "usage: ${0##*/} [+-vshgi] [+-d music-directory] [+-a album] [+-A artist] [+-o playlist] [--] search terms..."
                return
                ;;
            *)
                echo "usage: ${0##*/} [+-vshgi] [+-d music-directory] [+-a album] [+-A artist] [+-o playlist] [--] search terms..."
                return 2
                ;;
        esac
    done
    shift $((OPTIND-1))
    OPTIND=1
    if [ "$#" -gt "0" ]; then
        NAME="AND name:$*"
    fi
    if [[ ( -n $NAME ) || ((options_found -eq 1)) ]]; then
        mdfind -onlyin "$MUSICROOT" '(kind:music OR kind:movie)' "$NAME" "$AUTHOR" "$ALBUM" |
            grep -E --color=auto "$TYPEREGEX" |
            while read -r line; do
                echo "$line" | tee -a "$OUTPUT2" > "$OUTPUT1"
                if ((INFO)); then
                    afinfo "$line"
                fi
                if ((DOAPPLESCRIPT)); then
                    open -a 'QuickTime Player' "$line"
                    if ((PLAY)); then
                        osascript -e "$APPLESCRIPTPART1"
                        osascript -e "$APPLESCRIPTPART2"
                    fi
                elif ((PLAY)); then
                    # to get fplay to print the song names neatly, have it spawn a process that uses bash "wait" to wait for everything else to finish before echoing.

                    # not sure how to avoid it waiting for it's parent.
                    afplay "$line"
                fi
            done
    fi
}
