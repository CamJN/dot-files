#!/usr/bin/env -P /usr/local/bin:/opt/homebrew/bin bash

BLACKLIST=https://raw.githubusercontent.com/notracking/hosts-blocklists/master/dnsmasq/dnsmasq.blacklist.txt

WHITELIST=(ident.me appboy-images.com icanhazip.com cdn.braze.eu use-application-dns.net responsys.net)

/usr/bin/curl $BLACKLIST | /usr/bin/grep -Fv ${WHITELIST[@]/#/-e } | /usr/bin/sed -e '/^#/d' -e 's/address=/server=/' > ./dnsmasq.blacklist.txt.new

/usr/bin/find . -name dnsmasq.blacklist.txt.new \( -size +1M -and -exec bash -c '/bin/cat {} > ./dnsmasq.blacklist.txt' \; -or -true \) -delete
