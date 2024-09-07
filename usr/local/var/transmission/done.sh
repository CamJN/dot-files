#!/bin/sh

###
###  ENV vars
###

PREFIX="$(/usr/bin/env -P /usr/local/bin:/opt/homebrew/bin brew --prefix)"

exec >> "$PREFIX/var/log/transmission.done.log" 2>&1

PATH="$PREFIX/bin:$PATH"

# TR_APP_VERSION - Transmission's short version string, e.g. 4.0.0
# TR_TIME_LOCALTIME - Current time in this format: Thu Aug 15 21:09:36 2024
# TR_TORRENT_BYTES_DOWNLOADED - Number of bytes that were downloaded for this torrent
# TR_TORRENT_DIR - Location of the downloaded data
# TR_TORRENT_HASH - The torrent's info hash
# TR_TORRENT_ID - Transmission's ID for the torrent
# TR_TORRENT_LABELS - A comma-delimited list of the torrent's labels, may be empty
# TR_TORRENT_NAME - Name of torrent (not filename)
# TR_TORRENT_PRIORITY - The priority of the torrent (Low is "-1", Normal is "0", High is "1")
# TR_TORRENT_TRACKERS - A comma-delimited list of the torrent's trackers' announce URLs

echo "running \`transmission-remote --torrent $TR_TORRENT_ID --remove\`"
transmission-remote --torrent "$TR_TORRENT_ID" --remove
