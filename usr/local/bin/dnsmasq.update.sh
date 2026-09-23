#!/usr/bin/env -P /usr/local/bin:/opt/homebrew/bin bash
# shellcheck shell=bash

BLACKLIST=https://raw.githubusercontent.com/notracking/hosts-blocklists/master/dnsmasq/dnsmasq.blacklist.txt

WHITELIST=(ident.me appboy-images.com icanhazip.com cdn.braze.eu use-application-dns.net responsys.net)

declare flags=()
for item in "${WHITELIST[@]}"; do
    flags+=(-e "$item")
done

/usr/bin/curl $BLACKLIST | /usr/bin/grep -Fv "${flags[@]}" | /usr/bin/sed -e '/^#/d' -e 's/address=/server=/' > ./dnsmasq.blacklist.txt.new

/usr/bin/find . -name dnsmasq.blacklist.txt.new \( -size +1M -and -exec bash -c '/bin/cat "$1" > ./dnsmasq.blacklist.txt' bash {} \; -or -true \) -delete
