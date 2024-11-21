#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh)"

# pre-reqs:
# login app store
# copy over secrets file?
# copy over ssh keys?

# post-reqs:
# login icloud
# create VPN: install via profile file? how to provide password?
# set up 1password & cli
# set up mail, messages, other apple apps
# set up slack & front PWAs in safari
# set up firefox w/ userChrome.css & profile & addons
# install sketch & license

# wrap in a function to prevent partial execution if download fails
function main() {
# error-fast
set -xeuo pipefail
# ensure PATH includes likely dirs
export PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/local/sbin:/bin/:/sbin/:/usr/bin/:/usr/sbin/:$PATH"

if (diskutil info -plist "$(diskutil list internal | grep -Fe Data | awk '{print $NF}')" | plutil -extract FilesystemName raw - | grep -Fve Case-sensitive); then
    echo "Disk isn't case-sensitive, fix that before doing a bunch of work." >&2
    exit 1
fi

# Make a snapshot before making any changes
tmutil localsnapshot

# Ensure CLT installed
if [ ! -e /Library/Developer/CommandLineTools ]; then
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    declare LABEL
    LABEL="$(softwareupdate -l | grep -Ee "(Label:|\*) Command Line (Developer|Tools)" | awk -F"[:\*] " '{print $2}' | sort -Vr | head -1 | tr -d '\n')"
    softwareupdate --no-scan -i "$LABEL" --verbose
    rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
fi

# Ensure Bash dir exists in Developer dir
if [ ! -e ~/Developer/Bash ]; then
    mkdir -p ~/Developer/Bash
fi

export GIT_CEILING_DIRECTORIES=/Users
# Ensure repo installed
if [ ! -e ~/Developer/Bash/dot-files ]; then
    git clone git@github.com:CamJN/dot-files.git ~/Developer/Bash/dot-files
else
    # ensure clean repo by making temp commit with changes
    pushd ~/Developer/Bash/dot-files
    git diff --quiet --exit-code || git commit -am 'tmp'
    git diff --cached --quiet --exit-code || git commit -am 'tmp'
    # ensure up to date repo by rebasing on origin
    git pull -r
    popd
fi

# Ensure brew installed
if ! which -s brew ; then
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
HOMEBREW_PREFIX="$(brew --prefix)"
export HOMEBREW_PREFIX
export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"

# untap unwanted homebrew taps
comm -12 <(brew tap) <(grep -Fe untap "$HOMEBREW_BUNDLE_FILE" | cut -w -f3 | tr -d '"') | xargs -L 1 brew untap

# install all homebrew packages in Brewfile
# might require secrets file to be sourced...
# might require rest of my formula to be put in a tap...
brew bundle check || brew bundle install --verbose

# pin formulae that shouldn't be changed without care & attention
brew pin emacs tree-sitter dnsmasq llvm transmission-cli gnupg mailpit postgresql@16 colima lima

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
brew list --formulae -1 | grep -Fe docker- | xargs brew --prefix | xargs -J {} ln -shfF {} ~/.docker/cli-plugins/

# symlink homebrew's etc config files
for file in ~/Developer/Bash/dot-files/usr/local/etc/*; do
    if [ "${file##*/}" = "openssl" ]; then
        ln -shFf "openssl@3" "$HOMEBREW_PREFIX/etc/openssl"
        ln -shFf "$file/openssl.cnf" "$HOMEBREW_PREFIX/etc/openssl/openssl.cnf"
    else
        ln -shFf "$file" "$HOMEBREW_PREFIX/etc/${file##*/}"
    fi
done

# symlink homebrew bin executables
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
find ~/Developer/Bash/dot-files/usr/local/var -type f -exec sh -c 'ln -shFf $0 "'"$HOMEBREW_PREFIX"'"`dirname $0 | sed -e "s|'"$HOME/Developer/Bash/dot-files/usr/local"'||g"`/' {} \;

# check LaunchAgents for changes
getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchAgents/homebrew.mxcl.*.plist
# symlink LaunchAgents
ln -shFf ~/Developer/Bash/dot-files/Library/LaunchAgents/* ~/Library/LaunchAgents/
# Ensure keybindings dir exists
mkdir -p ~/Library/KeyBindings/
# symlink keybindings
ln -shFf ~/Developer/Bash/dot-files/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict

# ensure colima running
if ! colima status 2>&1 | grep -Fe 'colima is running' >/dev/null; then
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
    REMOTE="walle"
else
    echo "Unknown architecture: $(uname -m) please update docker-buildx section of script." >&2
    exit 1
fi
if ! docker buildx ls | grep -Fwe native_arch >/dev/null; then
    docker buildx create \
           --name local_remote_builder \
           --node native_arch \
           --platform "$NATIVE_DOCKER_PLATFORMS" \
           --driver-opt env.BUILDKIT_STEP_LOG_MAX_SIZE=-1 \
           --driver-opt env.BUILDKIT_STEP_LOG_MAX_SPEED=-1 \
           --driver-opt default-load=true
fi
if ! docker buildx ls | grep -Fwe non_native_arch >/dev/null; then
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

# check LaunchDaemons for changes
getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchDaemons/homebrew.mxcl.*.plist
# symlink LaunchDaemons
sudo chown root:wheel ~/Developer/Bash/dot-files/Library/LaunchDaemons/*
sudo ln -shFf ~/Developer/Bash/dot-files/Library/LaunchDaemons/* /Library/LaunchDaemons/
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
    echo "rbenv wasn't installed; is homebrew broken?" >&2
    exit 1
else
    declare RUBY_CONFIGURE_OPTS
    RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl)"
    export RUBY_CONFIGURE_OPTS
    eval "$(rbenv init -)";
    rbenv list | xargs -n1 rbenv install -s
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
# defaults write com.apple.screensaver askForPassword -int 1
# defaults write com.apple.screensaver askForPasswordDelay -int 0
# defaults write com.apple.Safari IncludeDevelopMenu -bool true
# defaults write com.apple.terminal SecureKeyboardEntry -bool true
# defaults write com.apple.terminal StringEncodings -array 4
# defaults write com.apple.ActivityMonitor IconType -int 5
# defaults write com.apple.ActivityMonitor ShowCategory -int 0
# defaults write com.apple.ActivityMonitor SortColumn -string "CPUUsage"
# defaults write com.apple.ActivityMonitor SortDirection -int 0
# defaults write com.apple.finder QLEnableTextSelection -bool true
# defaults write com.apple.finder NewWindowTarget -string "PfHm"
# defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"
# defaults write com.apple.TextEdit RichText -int 0
# defaults write com.apple.TextEdit PlainTextEncoding -int 4
# defaults write com.apple.TextEdit PlainTextEncodingForWrite -int 4
# defaults write NSGlobalDomain AppleKeyboardUIMode -int 3
# defaults write NSGlobalDomain AppleAccentColor -int 2
# defaults write NSGlobalDomain AppleActionOnDoubleClick -string Minimize
# defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
# defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
# defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
# defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true
# defaults write com.apple.Accessibility AccessibilityEnabled -bool true
# defaults write com.apple.Accessibility ApplicationAccessibilityEnabled -int 1
# defaults write com.apple.Accessibility GenericAccessibilityClientEnabled -int 1
# defaults write com.apple.Accessibility KeyRepeatDelay -real  0.5
# defaults write com.apple.Accessibility KeyRepeatEnabled -int 1
# defaults write com.apple.Accessibility KeyRepeatInterval -real 0.08
# whole com.apple.ActivityMonitor domain
# whole com.apple.AppleMultitouchMouse domain
# whole com.apple.AppleMultitouchTrackpad domain
# defaults write com.apple.AddressBook ABBirthDayVisible -bool true
# defaults write com.apple.AddressBook ABDefaultAddressCountryCode -string ca
# defaults write com.apple.AddressBook ABUserHasSelectedDefaultCountryCode -bool true

# show Library dir in home dir
chflags nohidden ~/Library

# set user's shell
if [ "$(dscl . -read ~/ UserShell)" != "UserShell: $HOMEBREW_PREFIX/bin/bash" ]; then
    # this might not work with /etc/shells having been reset to default to capture changes after updates...
    chsh -s "$HOMEBREW_PREFIX/bin/bash"
fi

declare TZ="America/Edmonton"
# second sudo below after || seems to trigger another prompt
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

# Last, causes restart
sudo softwareupdate --install --all --restart --agree-to-license
}

main
