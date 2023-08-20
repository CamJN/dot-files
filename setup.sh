#!/bin/bash

set -xeuo pipefail

# To run:
# curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh | /bin/bash

# ensure PATH includes likely dirs
PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/usr/local/sbin:/bin/:/sbin/:/usr/bin/:/usr/sbin/:$PATH"

if [ ! -e /Library/Developer/CommandLineTools ]; then
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    LABEL=$(softwareupdate -l | grep -E "(Label:|\*) Command Line (Developer|Tools)" | awk -F"[:\*] " '{print $2}' | sort -Vr | head -1 | tr -d '\n')
    #LABEL=$(softwareupdate -l | grep -B 1 -E "Command Line (Developer|Tools)" | awk -F"*" '/^ +\\*/ {print $2}' | sed 's/^ *//' | head -n1 | tr -d '\n')
    softwareupdate --no-scan -i "$LABEL" --verbose
    rm /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
fi
# alternatively
#open -a '/System/Library/CoreServices/Install Command Line Developer Tools.app'

if [ ! -e ~/Developer/Bash ]; then
    mkdir -p ~/Developer/Bash
fi
if [ ! -e ~/Developer/Bash/dot-files ]; then
    git clone git@github.com:CamJN/dot-files.git ~/Developer/Bash/dot-files
fi

if ! which -s brew ; then
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
HOMEBREW_PREFIX=$(brew --prefix)
export HOMEBREW_PREFIX
export HOMEBREW_BUNDLE_NO_LOCK=1
export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"
HOMEBREW_BUNDLE_BREW_SKIP="$(grep -F ignore-dependencies "$HOMEBREW_BUNDLE_FILE" | cut -w -f2 | tr -d '",')"
export HOMEBREW_BUNDLE_BREW_SKIP

brew tap | grep -Ff <(grep -F untap "$HOMEBREW_BUNDLE_FILE" | cut -w -f3 | tr -d '"') | xargs brew untap

(brew bundle check || brew bundle install --verbose) && brew list | grep -e emacs -e postgresql -e dnsmasq -e libpq -e llvm -e transmission-cli -e gnupg | xargs brew pin

brew doctor --list-checks | grep -Fve cask -e check_user_path_2 -e check_user_path_3 -e check_filesystem_case_sensitive -e check_for_unlinked_but_not_keg_only -e check_for_anaconda -e check_for_bitdefender -e check_for_pydistutils_cfg_in_home -e check_deleted_formula | xargs brew doctor

for file in ~/Developer/Bash/dot-files/.[!.]*; do
    if [ "${file##*/}" = ".git" ]; then
        continue
    fi
    ln -shFf "$file" "$HOME/${file##*/}"
done

for file in ~/Developer/Bash/dot-files/usr/local/etc/*; do
    if [ "${file##*/}" = "openssl" ]; then
        ln -shFf "$file/openssl.cnf" "$HOMEBREW_PREFIX/etc/openssl/openssl.cnf"
    else
        ln -shFf "$file" "$HOMEBREW_PREFIX/etc/${file##*/}"
    fi
done

sudo -v
find ~/Developer/Bash/dot-files/etc -type f \! -name '.DS_Store' -print0 | while IFS= read -r -d '' file; do
  sudo find /private/etc \
         -type f \
         -path "*/${file#"$HOME"/Developer/Bash/dot-files/etc/}" \
         -print0
done | sed -e 's|private/||g' | xargs -S 100000 -0 -I{} -t sudo sh -xc "mv '{}' '$HOME/Developer/Bash/dot-files{}'; ln -shFf '$HOME/Developer/Bash/dot-files{}' '{}'"

if which rbenv > /dev/null; then
    RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl)"
    export RUBY_CONFIGURE_OPTS
    eval "$(rbenv init -)";
fi

rbenv list | xargs -n1 rbenv install -s

#google-authenticator -t -d
#.google_authenticator  .passenger-enterprise-download-token

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
