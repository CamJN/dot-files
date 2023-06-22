#!/usr/local/bin/bash

BLACKLIST=https://raw.githubusercontent.com/notracking/hosts-blocklists/master/dnsmasq/dnsmasq.blacklist.txt

WHITELIST=(ident.me appboy-images.com icanhazip.com cdn.braze.eu use-application-dns.net responsys.net)

/usr/bin/curl $BLACKLIST | /usr/bin/fgrep -v ${WHITELIST[@]/#/-e } | sed -e '/^#/d' -e 's/address=/server=/' > ./dnsmasq.blacklist.txt
