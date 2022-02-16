#!/bin/bash

set -x
set -e
set -o pipefail

# To run:
# curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/setup.sh | /bin/bash

# ensure PATH includes likely dirs
PATH="/bin/:/usr/bin/:/usr/sbin/:/usr/local/bin:$PATH"

if [ ! -e /Library/Developer/CommandLineTools ]; then
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
    LABEL=$(softwareupdate -l | grep -E "(Label:|\*) Command Line (Developer|Tools)" | awk -F"[:\*] " '{print $2}' | sort -Vr | head -1 | tr -d '\n')
    #LABEL=$(softwareupdate -l | grep -B 1 -E "Command Line (Developer|Tools)" | awk -F"*" '/^ +\\*/ {print $2}' | sed 's/^ *//' | head -n1 | tr -d '\n')
    softwareupdate -i "$LABEL" --verbose
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

which -s brew
if [[ $? != 0 ]] ; then
    bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi
export HOMEBREW_BUNDLE_NO_LOCK=1
export HOMEBREW_BUNDLE_FILE="$HOME/Developer/Bash/dot-files/homebrew/Brewfile"
export HOMEBREW_BUNDLE_BREW_SKIP="$(grep -F ignore-dependencies $HOMEBREW_BUNDLE_FILE | awk '{print $2}' | tr -d '",')"
taps=$(brew tap)
for tap in $(grep -F untap $HOMEBREW_BUNDLE_FILE | awk '{print $NF}' | tr -d '",'); do
    if fgrep "$tap" <<< $TAPS >/dev/null ; then
        brew untap "$tap"
    fi
done

brew bundle check || brew bundle install --verbose && brew pin $( brew list | grep -e emacs -e postgresql -e dnsmasq -e libpq -e llvm -e mysql)

brew doctor $(brew doctor --list-checks | fgrep -ve cask -e check_user_path_2 -e check_user_path_3 -e check_filesystem_case_sensitive -e check_for_unlinked_but_not_keg_only -e check_for_anaconda -e check_for_bitdefender -e check_for_pydistutils_cfg_in_home)

for file in $(ls -d ~/Developer/Bash/dot-files/.[!.]*); do
    if [ "${file##*/}" = ".git" ]; then
        continue
    fi
    ln -shFf "$file" "$HOME/${file##*/}"
done

for file in $(ls -d ~/Developer/Bash/dot-files/usr/local/etc/*); do
    if [ "${file##*/}" = "openssl" ]; then
        ln -shFf "$file/openssl.cnf" "/usr/local/etc/openssl/openssl.cnf"
    else
        ln -shFf "$file" "/usr/local/etc/${file##*/}"
    fi
done

for file in $(find ~/Developer/Bash/dot-files/etc -type f \! -name '.DS_Store' ); do
    sudo find /private/etc \
         -type f \
         -path "*/${file#$HOME/Developer/Bash/dot-files/etc/}" \
         -print0 2>/dev/null | sed -e 's|private/||g' |
        xargs -0 -I{} -t sudo sh -c "mv '{}' '$HOME/Developer/Bash/dot-files{}'; ln -shFf '$HOME/Developer/Bash/dot-files{}' '{}'"
done

if which rbenv > /dev/null; then
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=/usr/local/opt/openssl"
    eval "$(rbenv init -)";
fi
for ver in $(rbenv install -l 2>/dev/null | egrep -e '^[0-9\.]+$'); do
    if rbenv list 2>/dev/null | fgrep $ver >/dev/null; then
        continue;
    fi
    /usr/local/bin/rbenv install $ver
done

#google-authenticator -t -d
#.google_authenticator  .passenger-enterprise-download-token

defaults write com.apple.finder QLEnableTextSelection -bool true
mkdir -p "$HOME/Pictures/Screenshots/"
defaults write com.apple.screencapture location -string "$HOME/Pictures/Screenshots/"
defaults write com.apple.Safari IncludeDevelopMenu -bool true
defaults write com.apple.terminal SecureKeyboardEntry -bool true
chflags nohidden ~/Library

# Last, causes restart
sudo softwareupdate -i -a --restart --agree-to-license
