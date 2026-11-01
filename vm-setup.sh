#!/bin/bash

# To run:
# bash -c "$(curl -fsSL https://raw.githubusercontent.com/CamJN/dot-files/master/vm-setup.sh)"

# pre-requisites: password in ~/password
# remote login: full disk access enabled (System Settings > General > Sharing > Remote Login > Allow full disk access for remote users

export PS4='+${LINENO}:${FUNCNAME[0]:+${FUNCNAME[0]}():}'

# error-fast
set -xeuo pipefail

sudo sysctl -w net.inet.tcp.tso=0

if [ "$(dscl -plist . -read "/Users/$USER" RealName | plutil -extract 'dsAttrTypeStandard:RealName.0' raw -)" != "Managed via Tart" ]; then
    echo '%admin ALL=(ALL) NOPASSWD: ALL' | sudo tee /etc/sudoers.d/vm-admin
    automationmodetool enable-automationmode-without-authentication
    defaults -currentHost write com.apple.screensaver idleTime -int 0
    sudo defaults write /Library/Preferences/com.apple.screensaver loginWindowIdleTime -int 0
    sudo systemsetup -setsleep Off
    sysadminctl -screenLock off -password -
fi

cat <<'EOF' | swift -
import AppKit

let wallpaper = URL(
    fileURLWithPath: "/System/Library/Desktop Pictures/Solid Colors/Black.png"
)

for screen in NSScreen.screens {
    do {
        try NSWorkspace.shared.setDesktopImageURL(
            wallpaper,
            for: screen,
            options: [:]
        )
    } catch {
        fputs("Failed to set wallpaper: \(error)\n", stderr)
        exit(1)
    }
}
EOF


defaults write com.apple.universalaccess reduceTransparency -bool true

export SKIP_CASE_CHECK=true # not setup in vm image
export SKIP_HOMEBREW_BUNDLE_APPS=true # can't login to mac app store
export SKIP_DOCKER=true # no nested virtualization
export SKIP_INSTALL_GETARGV=true
export SKIP_INSTALL_PASSENGER_ENTERPRISE=true
