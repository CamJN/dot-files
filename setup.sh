#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh)"

# error-fast
set -xeuo pipefail

# pre-reqs:
# make fs case sensitive
# login app store
# copy over secrets file
# copy over gpg key
# copy over ssh keys

# post-reqs:
# configure VPN
# set up 1password cli
# set up firefox w/ userChrome.css & profile & addons
# install sketch & license
# login to tower & git accounts
# make TLS CA & import to keychain
# setup TimeMachine over smb
# make code signing root cert
# login to things that store creds in keychain:
# - docker: redhat, ghcr, docker
# - gnupg agent
# - ssh agent
# - gh auth
# - git-osxkeychain
# - mvn master password

function fail() {
    echo "$*" >&2
    exit 1
}

# wrap in a function to prevent partial execution if download fails
function main() {
# ensure PATH includes likely dirs
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/local/sbin:/bin/:/sbin/:/usr/bin/:/usr/sbin/:$PATH"

if ! which -s pinentry-mac ; then
    GPG_TTY=$(tty)
    export GPG_TTY
fi

if (diskutil info -plist "$(diskutil list internal | grep -Fe Data | awk '{print $NF}')" | plutil -extract FilesystemName raw - | grep -Fve Case-sensitive); then
    fail "Disk isn't case-sensitive, fix that before doing a bunch of work."
fi

if ! [ -d "$HOME/.ssh" ]; then
    fail "ssh directory not found, please provide it at $HOME/.ssh"
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
if [ ! -d ~/Developer/Bash ]; then
    mkdir -p ~/Developer/Bash
fi

# Ensure Sites dir exists in home dir
if [ ! -d ~/Sites ]; then
    mkdir -p ~/Sites
fi

export GIT_CEILING_DIRECTORIES=/Users
# Ensure repo installed
if [ ! -e ~/Developer/Bash/dot-files ]; then
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
HOMEBREW_PREFIX="$(brew --prefix)"
export HOMEBREW_PREFIX
export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"

# untap unwanted homebrew taps
comm -12 <(brew tap) <(grep -Fe untap "$HOMEBREW_BUNDLE_FILE" | cut -w -f3 | tr -d '"') | xargs -L 1 brew untap

if [ -z "${SKIP_BUNDLE-}" ]; then
    if [ -z "${HOMEBREW_GITHUB_API_TOKEN}" ]; then
        fail "HOMEBREW_GITHUB_API_TOKEN env var is required, but not set."
    fi
    if ! [ -f "$HOME/.passenger-enterprise-download-token" ]; then
        fail "$HOME/.passenger-enterprise-download-token is required to continue."
    fi
    # install all homebrew packages in Brewfile
    brew bundle check || brew bundle install --verbose
fi

# make my tap have one location on disk
declare TAP_PATH
if [ "$(uname -m)" = "x86_64" ]; then
    TAP_PATH="${HOMEBREW_PREFIX}/Homebrew/Library/Taps/camjn/homebrew-fixed"
elif [ "$(uname -m)" = "arm64" ]; then
    TAP_PATH="${HOMEBREW_PREFIX}/Library/Taps/camjn/homebrew-fixed"
else
    fail "Unknown architecture: $(uname -m) please update homebrew taps section of script."
fi
if ! [ -L "$TAP_PATH" ]; then
    rm -rf "$TAP_PATH"
    ln -sf ~/Developer/Bash/dot-files/homebrew "$TAP_PATH"
fi

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
        ln -shFf "$file/nginx.conf" "$HOMEBREW_PREFIX/etc/nginx/nginx.conf"
        ln -shFf "$file/modules" "$HOMEBREW_PREFIX/etc/nginx/modules"
    else
        DIR="$HOMEBREW_PREFIX/etc/${file##*/}"
        if [ -e "$DIR" ]; then find "$DIR" -empty -print -delete; fi
        ln -shFf "$file" "$DIR"
    fi
done

# symlink executables
for file in ~/Developer/Bash/dot-files/usr/local/bin/*; do
    ln -shFf "$file" "$HOMEBREW_PREFIX/bin/${file##*/}"
done

function getLaunchdPlist() {
    for file in "$@"; do
        local filename
        filename=$(basename "$file")
        local formula="${filename%.plist}"
        formula="${formula#homebrew.mxcl.}"
        if [[ "$file" == *"/LaunchDaemons/"* ]]; then
            # shellcheck disable=SC2024
            # the read is non-root, tee is root to write
            sudo tee "$file" < "$HOMEBREW_PREFIX/opt/$formula/${filename}" >/dev/null
        else
            cat "$HOMEBREW_PREFIX/opt/$formula/${filename}" > "$file"
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
ln -shf ~/Developer/Bash/dot-files/Library/LaunchAgents/* ~/Library/LaunchAgents/
# Ensure keybindings dir exists
mkdir -p ~/Library/KeyBindings/
# symlink keybindings
ln -shFf ~/Developer/Bash/dot-files/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict

# ensure colima running
if [ "colima" != "$(colima status --json | jq -r .display_name)" ]; then
    colima start
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
while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &

# check LaunchDaemons for changes
getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchDaemons/homebrew.mxcl.*.plist
# symlink LaunchDaemons
sudo chown root:wheel ~/Developer/Bash/dot-files/Library/LaunchDaemons/*
sudo ln -shf ~/Developer/Bash/dot-files/Library/LaunchDaemons/* /Library/LaunchDaemons/
# check OS's etc config files for changes, and symlink them
find ~/Developer/Bash/dot-files/etc -type f \! \( -name '.DS_Store' -o -path '*paths.d/*' \) -print0 | while IFS= read -r -d '' file; do
    sudo find /private/etc \
         -type f \
         -path "*/${file#"$HOME"/Developer/Bash/dot-files/etc/}" \
         -print0
done | sed -e 's|private/||g' | xargs -S 100000 -0 -I{} -t sudo sh -xc "cat '{}' > '$HOME/Developer/Bash/dot-files{}'; ln -shFf '$HOME/Developer/Bash/dot-files{}' '{}'"
# copy paths files into paths dirs
sudo cp ~/Developer/Bash/dot-files/etc/paths.d/* /etc/paths.d/
sudo cp ~/Developer/Bash/dot-files/etc/manpaths.d/* /etc/manpaths.d/
# ensure rbenv installed and all current c-rubies
if ! which -s rbenv; then
    fail "rbenv wasn't installed; is homebrew broken?"
else
    declare RUBY_CONFIGURE_OPTS
    RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl)"
    export RUBY_CONFIGURE_OPTS
    eval "$(rbenv init -)";
    rbenv list | xargs -n1 rbenv install --skip-existing
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
    google-authenticator --no-confirm --time-based --disallow-reuse --force --secret=~/.google_authenticator --qr-mode=ansi
    sed -Ee 's/^[^#].*pam_google_authenticator.so$/#&/g' -e "s|^#(.*$HOMEBREW_PREFIX/lib/security/pam_google_authenticator.so)$|\1|" -i '' ./etc/pam.d/sshd
fi

# .bash.d/secrets.gpg
# .gnupg/ somehow setup gnupg and import my keys... maybe https://news.ycombinator.com/item?id=36953582 will help
# .passenger-enterprise-download-token
# .aws/config https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-envvars.html
# .m2/settings{-security,}.xml https://stackoverflow.com/questions/31251259/how-to-pass-maven-settings-via-environment-vars
# .ssh/
# .op/config

# set some defaults
# https://apple.stackexchange.com/questions/195244/concise-compact-list-of-all-defaults-currently-configured-and-their-values
mkdir -p "$HOME/Pictures/Screenshots/"
defaults write com.apple.screencapture location -string "$HOME/Pictures/Screenshots/"

defaults write NSGlobalDomain AppleAccentColor -int 2
# defaults write NSGlobalDomain AppleLocale -string "en_CA"
# defaults write NSGlobalDomain AppleLanguages -array "en-CA" "de-CA" "ja-CA"
defaults write NSGlobalDomain AppleKeyboardUIMode -int 2
defaults write NSGlobalDomain AppleActionOnDoubleClick -string Minimize
defaults write NSGlobalDomain "com.apple.sound.beep.sound" -string "/System/Library/Sounds/Funk.aiff"

defaults write com.apple.menuextra.battery ShowPercent -bool no

defaults write com.apple.preference.security.privacy limitAdTrackingCached -int 0
defaults write com.apple.AdLib forceLimitAdTracking -int 1
defaults write com.apple.AdLib "AD_DEVICE_IDFA" -string "00000000-0000-0000-0000-000000000000"
defaults write com.apple.AdLib allowApplePersonalizedAdvertising -int 0
defaults write com.apple.AdLib allowIdentifierForAdvertising -int 0
defaults write com.apple.AdLib personalizedAdsMigrated -int 0
#sudo chflags uchg ~/Library/Preferences/com.apple.AdLib # doesn't exist

defaults write com.apple.Safari IncludeDevelopMenu -int 1
defaults write com.apple.Safari AutoOpenSafeDownloads -int 0
defaults write com.apple.Safari MobileDeviceRemoteXPCEnabled -int 1
defaults write com.apple.Safari PrivateBrowsingRequiresAuthentication -int 1
# shellcheck disable=SC2016
defaults write com.apple.Safari NSUserKeyEquivalents -dict "Reload Page From Origin" '@$r' "Show JavaScript Console" "@~k"

defaults write com.apple.Terminal SecureKeyboardEntry -int 0
#open -a Terminal ~/Developer/Bash/dot-files/Library/Application\ Support/Terminal/My\ Homebrew.terminal
defaults write com.apple.Terminal "Default Window Settings" -string "My Homebrew"
defaults write com.apple.Terminal "Man Page Window Settings" -string "Man Page"
defaults write com.apple.Terminal "Startup Window Settings" -string "My Homebrew"

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

defaults write com.apple.AddressBook ABBirthDayVisible -bool true
defaults write com.apple.AddressBook ABDefaultAddressCountryCode -string ca
defaults write com.apple.AddressBook ABUserHasSelectedDefaultCountryCode -bool true

/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
#sudo nvram boot-args="mbasd=1" # enable macbook air superdrive

# show Library dir in home dir
chflags nohidden ~/Library

# set user's shell
if [ "$(dscl . -read ~/ UserShell)" != "UserShell: $HOMEBREW_PREFIX/bin/bash" ]; then
    # this might not work with /etc/shells having been reset to default to capture changes after updates...
    chsh -s "$HOMEBREW_PREFIX/bin/bash"
fi

declare TZ="America/Edmonton"
# second sudo below after || seems to trigger another prompt
# settimezone emits error -99 message, but seems to work
sudo systemsetup -gettimezone | grep -Fxe "Time Zone: $TZ" >/dev/null || sudo systemsetup -settimezone "$TZ"

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

if [ -z "$(<. sqlite3 -noheader '/Library/Application Support/com.apple.TCC/TCC.db' 'select client from access where client = "com.apple.Terminal" and auth_value > 0 and service = "kTCCServiceSystemPolicyAllFiles"')" ]; then
    spctl developer-mode enable-terminal
fi

if (security find-identity -v -p codesigning | rg '0 valid identities found'); then
    fail "No code-signing authority found, apache cannot load 3rd party modules."
else
    sudo apachectl -t && sudo apachectl restart
fi

# Last, causes restart
sudo softwareupdate --install --all --restart --agree-to-license
}

main
