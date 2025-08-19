#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh)"

# error-fast
set -xeuo pipefail

# pre-reqs:
# make fs case sensitive
# login to app store
# copy over secrets file
# copy over gpg keys
# copy over ssh keys
# copy over passenger enterprise download token

# post-reqs:
# allow Terminal full disk access
# configure VPN
# set up 1password cli: .config/op/copfig
# set global rbenv version
# make TLS CA & import to keychain & firefox
# setup aws: .aws/config https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-envvars.html
# set up firefox w/ userChrome.css & profile & addons
# import terminal theme & set as default
# install sketch & license
# login to tower & git accounts
# setup TimeMachine over smb
# make code signing root cert, sign apache modules
# install passenger enterprise license file
# login to things that store creds in keychain:
# - docker: redhat, ghcr, docker
# - gnupg agent
# - ssh agent
# - gh auth
# - git-osxkeychain

function fail() {
    echo "$*" >&2
    exit 1
}

# wrap in a function to prevent partial execution if download fails
function main() {
    # ensure PATH includes likely dirs
    export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/local/sbin:/bin/:/sbin/:/usr/bin/:/usr/sbin/:$PATH"

    if (diskutil info -plist "$(diskutil list internal | grep -Fe Data | awk '{print $NF}')" | plutil -extract FilesystemName raw - | grep -Fve Case-sensitive); then
        fail "Disk isn't case-sensitive, fix that before doing a bunch of work."
    fi

    if [ ! -d "$HOME/.ssh" ]; then
        fail "ssh directory not found, please provide it at $HOME/.ssh"
    fi

    if ! which -s pinentry-mac ; then
        GPG_TTY=$(tty)
        export GPG_TTY
    fi

    # Make a snapshot before making any changes
    tmutil localsnapshot

    # Ensure CLT installed
    if [ ! -e /Library/Developer/CommandLineTools ]; then
        touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
        declare LABEL
        LABEL="$(softwareupdate -l | grep -Ee "(Label:|\*) Command Line (Developer|Tools)" | awk -F"[:\*] " '{print $NF}' | sort -Vr | head -1 | tr -d '\n')"
        softwareupdate --no-scan -i "$LABEL" --verbose
        rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    fi

    # Ensure Bash dir exists in Developer dir
    if [ ! -d "$HOME/Developer/Bash" ]; then
        mkdir -p ~/Developer/Bash
    fi

    export GIT_CEILING_DIRECTORIES=/Users
    # Ensure repo installed
    if [ ! -e "$HOME/Developer/Bash/dot-files" ]; then
        git clone --recurse-submodules git@github.com:CamJN/dot-files.git ~/Developer/Bash/dot-files
    else
        # ensure clean repo by making temp commit with changes
        pushd ~/Developer/Bash/dot-files
        git diff --quiet --exit-code || git commit -am 'tmp'
        git diff --cached --quiet --exit-code || git commit -am 'tmp'
        # ensure up to date repo by rebasing on origin
        git pull --rebase --no-recurse-submodules origin master
        git submodule update --init --recursive
        popd
    fi

    # Ensure brew installed
    if ! which -s brew ; then
        NONINTERACTIVE=true bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    fi
    brew completions link
    HOMEBREW_PREFIX="$(brew --prefix)"
    export HOMEBREW_PREFIX
    export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"

    # untap unwanted homebrew taps
    comm -12 <(brew tap) <(grep -Fe untap "$HOMEBREW_BUNDLE_FILE" | cut -w -f3 | tr -d '"') | xargs -L 1 brew untap

    if [ -z "${SKIP_BUNDLE-}" ]; then
        if [ -z "${HOMEBREW_GITHUB_API_TOKEN}" ]; then
            fail "HOMEBREW_GITHUB_API_TOKEN env var is required, but not set."
        fi
        if [ ! -f "$HOME/.passenger-enterprise-download-token" ]; then
            fail "$HOME/.passenger-enterprise-download-token is required to continue."
        fi
        # install all homebrew packages in Brewfile
        brew bundle check || brew bundle install --verbose
    fi

    sudo chown "$(whoami)" "${HOMEBREW_PREFIX}/var/log/postgresql@17.log"
    find "${HOMEBREW_PREFIX}/Cellar" -path '*/bash_completion.d/*' -type f -exec ln -shf {} "${HOMEBREW_PREFIX}/etc/bash_completion.d/" \;

    # make my tap have one location on disk
    function unify_tap() {
        declare TAP_PATH
        if [ "$(uname -m)" = "x86_64" ]; then
            TAP_PATH="${HOMEBREW_PREFIX}/Homebrew/Library/Taps/$1"
        elif [ "$(uname -m)" = "arm64" ]; then
            TAP_PATH="${HOMEBREW_PREFIX}/Library/Taps/$1"
        else
            fail "Unknown architecture: $(uname -m) please update homebrew taps section of script."
        fi
        if [ ! -L "$TAP_PATH" ]; then
            rm -rf "$TAP_PATH"
            ln -shFf "$HOME/Developer/$2" "$TAP_PATH"
        fi
    }
    unify_tap camjn/homebrew-fixed Bash/dot-files/homebrew
    if [ ! -d "$HOME/Developer/Ruby/getargv-tap" ]; then
        mkdir -p "$HOME/Developer/Ruby"
        git clone git@github.com:getargv/homebrew-tap.git ~/Developer/Ruby/getargv-tap
    fi
    unify_tap getargv/homebrew-tap Ruby/getargv-tap

    # pin formulae that shouldn't be changed without care & attention
    brew pin emacs tree-sitter dnsmasq llvm transmission-cli gnupg mailpit postgresql@17 colima lima

    # Check if brew doctor has any new complaints
    if [ -z "${SKIP_DOCTOR-}" ]; then
        brew doctor --list-checks | grep -Fv -e check_user_path_2 -e check_user_path_3 -e check_filesystem_case_sensitive -e check_for_unlinked_but_not_keg_only -e check_for_anaconda -e check_for_bitdefender -e check_for_pydistutils_cfg_in_home -e check_deleted_formula | xargs brew doctor || (echo "To skip brew doctor, set SKIP_DOCTOR env var to something." && false)
    fi

    function link_dotfiles {
        local path="$1"
        shift
        local arr=("$@")
        for file in "${arr[@]}"; do
            local file_l1
            file_l1="$(basename "$file")"
            if [ "$file_l1" = ".git" ]; then
                # .git dir isn't a dot file it's the repo' git config dir
                continue
            fi
            if [ -h "$HOME/$path/$file_l1" ]; then
                # already linked
                continue
            fi
            if [ -d "$HOME/$path/$file_l1" ]; then
                # deal with dirs
                if [ "${path}" = "." ]; then
                    link_dotfiles "$file_l1" "${file}"/*
                else
                    link_dotfiles "$path/$file_l1" "${file}"/*
                fi
                continue
            fi
            ln -shFf "$file" "$HOME/$path/$file_l1"
        done
    }
    # symlink homedir dotfiles
    link_dotfiles . ~/Developer/Bash/dot-files/.[!.]*

    # ensure docker plugin dir exists
    mkdir -p ~/.docker/cli-plugins
    # symlink docker plugins
    brew list --formulae -1 | grep -Fe docker- | xargs brew --prefix | xargs -J {} ln -shf {} ~/.docker/cli-plugins/

    # symlink homebrew's etc config files
    for file in ~/Developer/Bash/dot-files/usr/local/etc/*; do
        if [ "${file##*/}" = "nginx" ]; then
            ln -shf "$file/nginx.conf" "$HOMEBREW_PREFIX/etc/nginx/nginx.conf"
            ln -shFf "$file/modules" "$HOMEBREW_PREFIX/etc/nginx/modules"
        else
            DIR="$HOMEBREW_PREFIX/etc/${file##*/}"
            if [ -e "$DIR" ]; then find "$DIR" -empty -print -delete; fi
            ln -shFf "$file" "$DIR"
        fi
    done

    # symlink executables
    for file in ~/Developer/Bash/dot-files/usr/local/bin/*; do
        ln -shf "$file" "$HOMEBREW_PREFIX/bin/"
    done

    function getLaunchdPlist() {
        for file in "$@"; do
            local filename
            filename=$(basename "$file")
            local formula="${filename%.plist}"
            formula="${formula#homebrew.mxcl.}"
            if [[ "$file" == *"/LaunchDaemons/"* ]]; then
                # the read is non-root, tee is root to write
                if [ -f "$HOMEBREW_PREFIX/opt/$formula/${filename}" ]; then
                    # shellcheck disable=SC2024
                    sudo tee "$file" < "$HOMEBREW_PREFIX/opt/$formula/${filename}" >/dev/null
                else
                    echo "$HOMEBREW_PREFIX/opt/$formula/${filename} doesn't exist, not checking for updates."
                fi
            else
                cat "$HOMEBREW_PREFIX/opt/$formula/${filename}" > "$file"
            fi
            # weird quoting in $file expansion is necessary
            if diff -q <(git diff "$file") "saved_diffs/${file#"$HOME/Developer/Bash/dot-files/"}"; then
                git restore "$file"
                if [[ "$file" == *"/LaunchDaemons/"* ]]; then
                    sudo chown root:wheel "$file"
                fi
            fi
        done
    }

    # link var config files
    find ~/Developer/Bash/dot-files/usr/local/var -type f -exec sh -c 'ln -shf $0 "'"$HOMEBREW_PREFIX"'"`dirname $0 | sed -e "s|'"$HOME/Developer/Bash/dot-files/usr/local"'||g"`/' {} \;

    # ensure LaunchAgents path exists
    mkdir -p ~/Library/LaunchAgents
    # check LaunchAgents for changes
    getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchAgents/homebrew.mxcl.*.plist
    # symlink LaunchAgents
    ln -shf ~/Developer/Bash/dot-files/Library/LaunchAgents/homebrew.* ~/Library/LaunchAgents/
    ln -shf ~/Developer/Bash/dot-files/Library/LaunchAgents/local.all.* ~/Library/LaunchAgents/
    for agent in "$HOME/Developer/Bash/dot-files/Library/LaunchAgents/local.$(scutil --get LocalHostName | tr '[:upper:]' '[:lower:]')".* ; do
        ln -shf "$agent" ~/Library/LaunchAgents/
    done
    # Ensure keybindings dir exists
    mkdir -p ~/Library/KeyBindings/
    # symlink keybindings
    ln -shf ~/Developer/Bash/dot-files/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict

    # ensure colima running
    if [ "colima" != "$(colima status --json | jq -r .display_name)" ]; then
        colima start --template default
    fi

    declare ARM_PLATFORMS=linux/arm64,linux/arm/v8,linux/arm/v7,linux/arm/v6
    declare INTEL_PLATFORMS=linux/amd64,linux/amd64/v2,linux/amd64/v3,linux/386
    #declare OTHER_PLATFORMS=linux/riscv64,linux/ppc64le,linux/s390x,linux/mips64le,linux/mips64
    declare NATIVE_DOCKER_PLATFORMS
    declare NON_NATIVE_DOCKER_PLATFORMS
    if [ "$(uname -m)" = "x86_64" ]; then
        NATIVE_DOCKER_PLATFORMS="$INTEL_PLATFORMS"
        NON_NATIVE_DOCKER_PLATFORMS="$ARM_PLATFORMS"
        REMOTE="eve"
    elif [ "$(uname -m)" = "arm64" ]; then
        NATIVE_DOCKER_PLATFORMS="$ARM_PLATFORMS"
        NON_NATIVE_DOCKER_PLATFORMS="$INTEL_PLATFORMS"
        REMOTE="wall-a"
    else
        fail "Unknown architecture: $(uname -m) please update docker-buildx section of script."
    fi
    declare builders
    builders=$(docker buildx ls)
    if ! grep -Fwe native_arch >/dev/null <<< "$builders"; then
        docker buildx create \
               --name local_remote_builder \
               --node native_arch \
               --platform "$NATIVE_DOCKER_PLATFORMS" \
               --driver-opt env.BUILDKIT_STEP_LOG_MAX_SIZE=-1 \
               --driver-opt env.BUILDKIT_STEP_LOG_MAX_SPEED=-1 \
               --driver-opt default-load=true
    fi
    if ! grep -Fwe non_native_arch >/dev/null <<< "$builders"; then
        docker buildx create \
               --name local_remote_builder \
               --append \
               --node non_native_arch \
               --platform "$NON_NATIVE_DOCKER_PLATFORMS" \
               "ssh://$REMOTE" \
               --driver-opt env.BUILDKIT_STEP_LOG_MAX_SIZE=-1 \
               --driver-opt env.BUILDKIT_STEP_LOG_MAX_SPEED=-1 \
               --driver-opt default-load=true
    fi
    # ensure docker buildx build-driver is non-default, as the default truncates logs
    if ! docker buildx inspect | grep -Ee 'Name:[[:space:]]+local_remote_builder' >/dev/null; then
        docker buildx use --default local_remote_builder
    fi

    # cache sudo auth
    sudo -v
    while kill -0 "$$"; do sudo -n true; sleep 60; done 2>/dev/null &

    declare COMPNAME
    COMPNAME="$(scutil --get ComputerName)"
    declare DEFAULT_NAME
    DEFAULT_NAME="$(id -F)'s $(system_profiler SPHardwareDataType -json | jq -r '.SPHardwareDataType[0].machine_name')"
    if [ "$COMPNAME" = "$DEFAULT_NAME" ]; then
        read -rp 'Set computer name to: ' COMPNAME
    fi
    declare COMPNAME_SAFE
    COMPNAME_SAFE="$(LANG=C tr -cd '[:print:]' <<< "$COMPNAME")"
    scutil --get LocalHostName | grep -Fxe "$COMPNAME_SAFE" >/dev/null || sudo scutil --set LocalHostName "$COMPNAME_SAFE"
    scutil --get ComputerName | grep -Fxe "$COMPNAME" >/dev/null || sudo scutil --set ComputerName "$COMPNAME"

    # Ensure Sites dir exists in home dir
    if [ ! -d "$HOME/Sites" ]; then
        mkdir -p ~/Sites
    fi
    chmod -R 770 ~/Sites
    if [ "$(stat -f '%g' ~/Sites/)" -ne "$(dscl . -read /Groups/_www PrimaryGroupID | awk '{print $NF}')" ]; then
        sudo chgrp -R _www ~/Sites
    fi

    # check LaunchDaemons for changes
    getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchDaemons/homebrew.mxcl.*.plist
    # symlink LaunchDaemons
    sudo chown root:wheel ~/Developer/Bash/dot-files/Library/LaunchDaemons/*
    sudo ln -shf ~/Developer/Bash/dot-files/Library/LaunchDaemons/homebrew.* /Library/LaunchDaemons/
    sudo ln -shf ~/Developer/Bash/dot-files/Library/LaunchDaemons/local.all.* /Library/LaunchDaemons/
    for daemon in "$HOME/Developer/Bash/dot-files/Library/LaunchDaemons/local.$(scutil --get LocalHostName | tr '[:upper:]' '[:lower:]')".* ; do
        sudo ln -shf "$daemon" /Library/LaunchDaemons/
    done

    pg_isready -q || sudo launchctl load /Library/LaunchDaemons/homebrew.mxcl.postgresql@17.plist
    local wait_count=0
    until pg_isready -q || [ $wait_count -gt 5 ]; do
        ((wait_count+=1))
        echo "Waiting for postgresql to come up"
        sleep 1
    done
    psql "$(whoami)" -c '\q' 2>/dev/null || createdb

    mkdir -p "${HOMEBREW_PREFIX}/var/log/dnsmasq"
    chmod 755 ~ # for _www (httpd) and nobody (dnsmasq) to read symlinks.
    sudo mkdir -p /etc/resolver/

    # check OS's etc config files for changes, and symlink them
    find ~/Developer/Bash/dot-files/etc -type f \! -path '*paths.d/*' -print0 | while IFS= read -r -d '' file; do
        # weird quoting in $file expansion is necessary
        DIR="/${file#"$HOME/Developer/Bash/dot-files/"}"
        # if DIR is a file and is not a symlink
        if [ -f "$DIR" ] && [ ! -h "$DIR" ]; then
            cat "$DIR" > "$file"
        fi
        sudo ln -shf "$file" "$DIR"
    done

    # copy paths files into paths dirs, cannot be symlinks for some reason
    sudo cp ~/Developer/Bash/dot-files/etc/paths.d/* /etc/paths.d/
    sudo cp "${HOMEBREW_PREFIX}/etc/paths" /etc/paths.d/homebrew
    sudo cp ~/Developer/Bash/dot-files/etc/manpaths.d/* /etc/manpaths.d/
    # ensure rbenv installed and all current c-rubies
    if ! which -s rbenv; then
        fail "rbenv wasn't installed; is homebrew broken?"
    else
        declare RUBY_CONFIGURE_OPTS
        RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl)"
        export RUBY_CONFIGURE_OPTS
        # shellcheck disable=SC1090
        source <(rbenv init -)
        rbenv list | xargs -n1 rbenv install --skip-existing
    fi

    # ensure swiftly initialized
    export SWIFTLY_HOME_DIR="$HOME/.config/swiftly"
    export SWIFTLY_BIN_DIR="$HOME/.config/swiftly/bin"
    export SWIFTLY_TOOLCHAINS_DIR="$HOME/Library/Developer/Toolchains"

    if [ ! -e "$SWIFTLY_HOME_DIR/env.sh" ]; then
        swiftly init --assume-yes --no-modify-profile
    fi
    # shellcheck source=.config/swiftly/env.sh
    source "$SWIFTLY_HOME_DIR/env.sh"
    swiftly update latest --assume-yes

    # ensure rustup initialized
    if [ "$(uname -m)" = "x86_64" ]; then
            rustup-init -y --no-modify-path --default-host x86_64-apple-darwin --default-toolchain stable
            rustup toolchain list | ( grep -Fve x86_64 || true ) | xargs rustup toolchain uninstall
    elif [ "$(uname -m)" = "arm64" ]; then
            rustup-init -y --no-modify-path --default-host aarch64-apple-darwin --default-toolchain stable
            rustup toolchain list | ( grep -Fve aarch64 || true ) | xargs rustup toolchain uninstall
    else
        fail "Unknown architecture: $(uname -m) please update rustup section of script."
    fi
    # ensure rust up to date
    rustup update
    # ensure rust has all macOS and wasm targets installed
    rustup target list | grep -Fe darwin | cut -wf1 | xargs rustup target add wasm32-unknown-unknown

    # ensure local dns network location exists
    if ! networksetup -listlocations | grep -Fxe 'Local DNS' >/dev/null; then
        networksetup -createlocation 'Local DNS' populate
    fi

    # ensure local dns network location in use
    if [ "$(networksetup -getcurrentlocation)" != 'Local DNS' ]; then
        networksetup -switchtolocation 'Local DNS'
    fi

    # set local dns network location DNS to localhost
    networksetup -listnetworkserviceorder | grep -Ee '^\([0-9]+\)' | grep -Fve 'VPN' | cut -f 2- -d ' ' | xargs -I{} networksetup -setdnsservers "{}" 127.0.0.1

    # ensure google-authenticator setup for account
    if [ ! -e "$HOME/.google_authenticator" ]; then
        google-authenticator --no-confirm --time-based --disallow-reuse --force --secret="$HOME/.google_authenticator" --qr-mode=ansi
        sed -Ee 's/^[^#].*pam_google_authenticator.so$/#&/g' -e "s|^#(.*$HOMEBREW_PREFIX/lib/security/pam_google_authenticator.so)$|\1|" -i '' "$HOME/Developer/Bash/dot-files/etc/pam.d/sshd"
    fi

    # set some defaults
    mkdir -p "$HOME/Pictures/Screenshots/"
    defaults write com.apple.screencapture location -string "$HOME/Pictures/Screenshots/"

    defaults write NSGlobalDomain AppleAccentColor -int 2
    defaults write NSGlobalDomain AppleKeyboardUIMode -int 2
    defaults write NSGlobalDomain AppleActionOnDoubleClick -string Minimize
    defaults write NSGlobalDomain "com.apple.sound.beep.sound" -string "/System/Library/Sounds/Funk.aiff"

    defaults write com.apple.menuextra.battery ShowPercent -bool no

    defaults write com.apple.WindowManager StandardHideWidgets -bool true
    defaults write com.apple.WindowManager EnableStandardClickToShowDesktop -bool false
    defaults write com.apple.WindowManager EnableTiledWindowMargins -bool false

    defaults write com.apple.dock largesize -float 70
    defaults write com.apple.dock magnification -bool true
    defaults write com.apple.dock minimize-to-application -bool true
    defaults write com.apple.dock show-process-indicators -bool false
    defaults write com.apple.dock showMissionControlGestureEnabled -bool true
    defaults write com.apple.dock wvous-br-corner -int 1
    defaults write com.apple.dock wvous-br-modifier -int 0
    defaults write com.apple.dock wvous-tr-corner -int 12
    defaults write com.apple.dock wvous-tr-modifier -int 0

    defaults write com.apple.preference.security.privacy limitAdTrackingCached -int 0
    defaults write com.apple.AdLib forceLimitAdTracking -int 1
    defaults write com.apple.AdLib "AD_DEVICE_IDFA" -string "00000000-0000-0000-0000-000000000000"
    defaults write com.apple.AdLib allowApplePersonalizedAdvertising -int 0
    defaults write com.apple.AdLib allowIdentifierForAdvertising -bool false
    defaults write com.apple.AdLib personalizedAdsMigrated -int 0
    defaults write com.apple.AdLib CKDPIDSyncState -int 0

    defaults write com.apple.Terminal SecureKeyboardEntry -int 0
    defaults write com.apple.Terminal "Default Window Settings" -string "My Homebrew"
    defaults write com.apple.Terminal "Man Page Window Settings" -string "Man Page"
    defaults write com.apple.Terminal "Startup Window Settings" -string "My Homebrew"
    declare terminal_settings
    terminal_settings=$(defaults export com.apple.Terminal -)
    declare -a themes
    IFS=$'\n' themes=($(plutil -extract 'Window Settings' xml1 -o - - <<< "$terminal_settings" | xmllint --xpath '/plist/dict/key/node()' -))
    for theme in "${themes[@]}"; do
        terminal_settings=$(plutil -replace "Window Settings.$theme.useOptionAsMetaKey" -bool true -o - - <<< "$terminal_settings")
    done
    defaults import com.apple.Terminal - <<< "$terminal_settings"

    defaults write com.apple.ActivityMonitor IconType -int 5
    defaults write com.apple.ActivityMonitor UpdatePeriod -int 1

    defaults write com.apple.finder QLEnableTextSelection -bool true
    defaults write com.apple.finder NewWindowTarget -string "PfHm"
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
    defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -int 1
    defaults write com.apple.finder ShowHardDrivesOnDesktop -int 0
    defaults write com.apple.finder ShowMountedServersOnDesktop -int 1
    defaults write com.apple.finder ShowRemovableMediaOnDesktop -int 1

    defaults write com.apple.TextEdit RichText -int 0
    defaults write com.apple.TextEdit CheckGrammarWithSpelling -int 1
    defaults write com.apple.TextEdit CheckSpellingAsYouTypeEnabledInRichTextOnly -int 1
    defaults write com.apple.TextEdit DataDetectors -int 1
    defaults write com.apple.TextEdit IgnoreHTML -int 1
    defaults write com.apple.TextEdit SmartLinks -int 1

    defaults write com.apple.Accessibility KeyRepeatEnabled -int 1
    defaults write com.apple.Accessibility FullKeyboardAccessFocusRingEnabled -int 1

    defaults write com.apple.Safari.SandboxBroker ShowDevelopMenu -bool true
    defaults write com.apple.Safari IncludeDevelopMenu -bool true
    defaults write com.apple.Safari PrivateBrowsingRequiresAuthentication -bool true
    defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
    defaults write com.apple.Safari WebKitPreferences.developerExtrasEnabled -bool true
    # shellcheck disable=SC2016
    defaults write com.apple.Safari NSUserKeyEquivalents -dict 'Reload Page From Origin' '@$r' 'Show Javascript Console' '@~k'

    defaults write com.apple.Passwords EnableMenuBarExtra -bool true
    defaults write com.apple.Passwords ShowServiceNamesInPasswords -bool true

    defaults write com.apple.onetimepasscodes DeleteVerificationCodes -bool true

    defaults write com.apple.MobileSMS DeleteVerificationCodes -bool true
    defaults write com.apple.MobileSMS KeepMessageForDays -int 0

    defaults write com.apple.mail FavoriteMailboxDraftsAutomaticallyAdded -bool true
    defaults write com.apple.mail FavoriteMailboxFlaggedAutomaticallyAdded -bool true
    defaults write com.apple.mail FavoriteMailboxInboxAutomaticallyAdded -bool true
    defaults write com.apple.mail FavoriteMailboxSentAutomaticallyAdded -bool true
    defaults write com.apple.mail FavoriteMailboxVIPsAutomaticallyAdded -bool true
    defaults write com.apple.mail SpellCheckingBehavior -string InlineSpellCheckingEnabled
    defaults write com.apple.mail EnableContactPhotos -int 1

    defaults write com.apple.FaceTime FaceTimeIsAlwaysOnTop -bool true

    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad HIDScrollZoomModifierMask -int 786432
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerHorizSwipeGesture -int 1
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad TrackpadThreeFingerVertSwipeGesture -int 1
    defaults write com.apple.AppleMultitouchTrackpad HIDScrollZoomModifierMask -int 786432
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerHorizSwipeGesture -int 1
    defaults write com.apple.AppleMultitouchTrackpad TrackpadThreeFingerVertSwipeGesture -int 1

    /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u

    # show Library dir in home dir
    chflags nohidden ~/Library

    # set user's shell
    if [ "$(dscl . -read ~/ UserShell)" != "UserShell: $HOMEBREW_PREFIX/bin/bash" ]; then
        if diff -q <(git diff etc/shells) saved_diffs/etc/shells; then
            git restore etc/shells
        fi
        chsh -s "$HOMEBREW_PREFIX/bin/bash"
    fi

    declare TZ="America/Edmonton"
    # second sudo below after || seems to trigger another prompt
    # settimezone emits error -99 message, but seems to work
    sudo systemsetup -gettimezone | grep -Fxe "Time Zone: $TZ" >/dev/null || sudo systemsetup -settimezone "$TZ"

    if [ -z "$(<. sqlite3 -noheader '/Library/Application Support/com.apple.TCC/TCC.db' 'select client from access where client = "com.apple.Terminal" and auth_value > 0 and service = "kTCCServiceSystemPolicyAllFiles"')" ]; then
        spctl developer-mode enable-terminal
    fi

    if (security find-identity -v -p codesigning | grep -q '0 valid identities found'); then
        fail "No code-signing authority found, apache cannot load 3rd party modules."
    else
        sudo apachectl -t && sudo apachectl restart
    fi

    # Last, causes restart
    sudo softwareupdate --install --all --restart --agree-to-license
}

main
