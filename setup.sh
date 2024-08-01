#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh)"

function main() {
set -xeuo pipefail
# ensure PATH includes likely dirs
PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/local/sbin:/bin/:/sbin/:/usr/bin/:/usr/sbin/:$PATH"

tmutil localsnapshot

if [ ! -e /Library/Developer/CommandLineTools ]; then
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    LABEL=$(softwareupdate -l | grep -E "(Label:|\*) Command Line (Developer|Tools)" | awk -F"[:\*] " '{print $2}' | sort -Vr | head -1 | tr -d '\n')
    #LABEL=$(softwareupdate -l | grep -B 1 -E "Command Line (Developer|Tools)" | awk -F"*" '/^ +\\*/ {print $2}' | sed 's/^ *//' | head -n1 | tr -d '\n')
    softwareupdate --no-scan -i "$LABEL" --verbose
    rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    # alternatively
    #open -a '/System/Library/CoreServices/Install Command Line Developer Tools.app'
fi

if [ ! -e ~/Developer/Bash ]; then
    mkdir -p ~/Developer/Bash
fi
if [ ! -e ~/Developer/Bash/dot-files ]; then
    git clone git@github.com:CamJN/dot-files.git ~/Developer/Bash/dot-files
else
    pushd ~/Developer/Bash/dot-files
    git diff --exit-code 2>/dev/null || git commit -am 'tmp'
    git pull -r
    popd
fi

if ! which -s brew ; then
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
HOMEBREW_PREFIX=$(brew --prefix)
export HOMEBREW_PREFIX
export HOMEBREW_BUNDLE_NO_LOCK=1
export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"
HOMEBREW_BUNDLE_BREW_SKIP="$(grep -F ignore-dependencies "$HOMEBREW_BUNDLE_FILE" | cut -w -f2 | tr -d '",')" || true
export HOMEBREW_BUNDLE_BREW_SKIP

comm -12 <(brew tap) <(grep -F untap "$HOMEBREW_BUNDLE_FILE" | cut -w -f3 | tr -d '"') | xargs -L 1 brew untap

brew bundle check || brew bundle install --verbose

brew pin emacs tree-sitter dnsmasq llvm transmission-cli gnupg mailpit postgresql@16 colima

if [ -z "${SKIP_DOCTOR-}" ]; then
    brew doctor --list-checks | grep -Fv -e check_user_path_2 -e check_user_path_3 -e check_filesystem_case_sensitive -e check_for_unlinked_but_not_keg_only -e check_for_anaconda -e check_for_bitdefender -e check_for_pydistutils_cfg_in_home -e check_deleted_formula | xargs brew doctor
fi

function link_dotfiles {
    local path="$1"
    shift
    local arr=("$@")
    for file in "${arr[@]}"; do
        local file_l1
        file_l1="$(basename "$file")"
        if [ "$file_l1" = ".git" ]; then
            continue
        fi
        if [ -h "$HOME/$path/$file_l1" ]; then
            # already linked
            continue
        fi
        if [ -d "$HOME/$path/$file_l1" ]; then
            # deal with dirs
            if [ "$path" -eq "." ]; then
                link_dotfiles "$file_l1" "${file}"/*
            else
                link_dotfiles "$path/$file_l1" "${file}"/*
            fi
            continue
        fi
        ln -shFf "$file" "$HOME/$path/$file_l1"
    done
}
link_dotfiles . ~/Developer/Bash/dot-files/.[!.]*

mkdir -p ~/.docker/cli-plugins
brew list --formulae -1 | grep -Fe docker- | xargs brew --prefix | xargs -J {} ln -shfF {} ~/.docker/cli-plugins/

for file in ~/Developer/Bash/dot-files/usr/local/etc/*; do
    if [ "${file##*/}" = "openssl" ]; then
        ln -shFf "openssl@3" "$HOMEBREW_PREFIX/etc/openssl"
        ln -shFf "$file/openssl.cnf" "$HOMEBREW_PREFIX/etc/openssl/openssl.cnf"
    else
        ln -shFf "$file" "$HOMEBREW_PREFIX/etc/${file##*/}"
    fi
done

for file in ~/Developer/Bash/dot-files/usr/local/bin/*; do
    ln -shFf "$file" "$HOMEBREW_PREFIX/bin/${file##*/}"
done

function getLaunchdPlist() {
    for file in "$@"; do
        filename=$(basename "$file")
        formula="${filename%.plist}"
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

ln -shFf ~/Developer/Bash/dot-files/usr/local/var/postgresql@*/postgresql.conf "$HOMEBREW_PREFIX"/var/postgresql@*/postgresql.conf
getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchAgents/homebrew.mxcl.*.plist
ln -shFf ~/Developer/Bash/dot-files/Library/LaunchAgents/* ~/Library/LaunchAgents/
mkdir -p ~/Library/KeyBindings/
ln -shFf ~/Developer/Bash/dot-files/Library/KeyBindings/DefaultKeyBinding.dict ~/Library/KeyBindings/DefaultKeyBinding.dict

colima start
docker buildx create --driver-opt env.BUILDKIT_STEP_LOG_MAX_SIZE=-1,env.BUILDKIT_STEP_LOG_MAX_SPEED=-1,default-load=true --use

sudo -v

getLaunchdPlist ~/Developer/Bash/dot-files/Library/LaunchDaemons/homebrew.mxcl.*.plist
sudo ln -shFf ~/Developer/Bash/dot-files/Library/LaunchDaemons/* /Library/LaunchDaemons/
find ~/Developer/Bash/dot-files/etc -type f \! \( -name '.DS_Store' -o -path '*paths.d/*' \) -print0 | while IFS= read -r -d '' file; do
    sudo find /private/etc \
         -type f \
         -path "*/${file#"$HOME"/Developer/Bash/dot-files/etc/}" \
         -print0
done | sed -e 's|private/||g' | xargs -S 100000 -0 -I{} -t sudo sh -xc "mv '{}' '$HOME/Developer/Bash/dot-files{}'; ln -shFf '$HOME/Developer/Bash/dot-files{}' '{}'"
sudo cp ~/Developer/Bash/dot-files/etc/paths.d/* /etc/paths.d/
sudo cp ~/Developer/Bash/dot-files/etc/manpaths.d/* /etc/manpaths.d/
if ! which -s rbenv; then
    echo "rbenv wasn't installed; is homebrew broken?" >&2
    exit 1
else
    RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl)"
    export RUBY_CONFIGURE_OPTS
    eval "$(rbenv init -)";
    rbenv list | xargs -n1 rbenv install -s
fi

rustup update
rustup target list | rg darwin | cut -wf1 | xargs rustup target add wasm32-unknown-unknown

if ! ( networksetup -listlocations | grep -Fqe 'Local DNS' ); then
    POPULATE=populate
    networksetup -createlocation 'Local DNS' $POPULATE
fi

if [ "$(networksetup -getcurrentlocation)" != 'Local DNS' ]; then
    networksetup -switchtolocation 'Local DNS'
fi

networksetup -listnetworkserviceorder | grep -Ee '^\([0-9]+\)' | grep -Fve 'VPN' | cut -f 2- -d ' ' | xargs -I{} networksetup -setdnsservers "{}" 127.0.0.1

if [ ! -e $HOME/.google_authenticator ]; then
    google-authenticator --no-confirm --time-based --disallow-reuse --force --secret=~/.google_authenticator --qr-mode=ansi
    # select correct pam_google_authenticator.so path in ./etc/pam.d/sshd
fi

# mkdir -p "$HOME/Pictures/Screenshots/"
# defaults write com.apple.screencapture location -string "$HOME/Pictures/Screenshots/"
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

# chflags nohidden ~/Library

# sudo systemsetup -settimezone "America/Edmonton"

# # Last, causes restart
# sudo softwareupdate -i -a --restart --agree-to-license
}

main
