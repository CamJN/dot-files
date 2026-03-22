#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/vm-setup.sh)"

# pre-requisites: password in ~/password
# remote login: full disk access enabled (System Settings > General > Sharing > Remote Login > Allow full disk access for remote users

export PS4='+${LINENO}:${FUNCNAME[0]:+${FUNCNAME[0]}():}'

# error-fast
set -xeuo pipefail

function update_tcc_database() {
    sudo sqlite3 "$1" <<-'EOF'
	INSERT OR REPLACE
	INTO access (
	  service,
	  client_type,
	  client,
	  auth_value,
	  auth_reason,
	  auth_version,
	  indirect_object_identifier_type,
	  indirect_object_identifier
	) VALUES
	('kTCCServiceAccessibility', 1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, NULL, 'UNUSED'),
	('kTCCServiceScreenCapture', 1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, NULL, 'UNUSED'),
	('kTCCServicePostEvent',     1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, NULL, 'UNUSED'),
	('kTCCServiceAppleEvents',   1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, 0, 'com.apple.systemevents'),
	('kTCCServiceAppleEvents',   1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, 0, 'com.apple.Safari'),
	('kTCCServiceAppleEvents',   1, '/usr/libexec/sshd-keygen-wrapper', 2, 0, 1, 0, 'com.apple.Music');
	EOF
}

update_tcc_database "${HOME}/Library/Application Support/com.apple.TCC/TCC.db"

# Must be after tcc database
osascript -e 'tell application "System Events" to tell every desktop to set picture to "/System/Library/Desktop Pictures/Solid Colors/Black.png" as POSIX file'

automationmodetool enable-automationmode-without-authentication
sudo sysctl -w net.inet.tcp.tso=0
defaults write com.apple.universalaccess reduceTransparency -bool true
defaults -currentHost write com.apple.screensaver idleTime -int 0
sudo defaults write /Library/Preferences/com.apple.screensaver loginWindowIdleTime -int 0
sudo systemsetup -setsleep Off
sysadminctl -screenLock off -password "$(cat ~/password)"

export SKIP_CASE_CHECK=true # not setup in vm image
export SKIP_HOMEBREW_BUNDLE_APPS=true # can't login to mac app store
export SKIP_DOCKER=true # no nested virtualization
export SKIP_INSTALL_GETARGV=true
