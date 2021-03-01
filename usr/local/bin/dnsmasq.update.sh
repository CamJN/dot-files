#!/usr/local/bin/bash

BLACKLIST=https://raw.githubusercontent.com/notracking/hosts-blocklists/master/dnsmasq/dnsmasq.blacklist.txt

WHITELIST=(ident.me appboy-images.com icanhazip.com)

/usr/bin/curl $BLACKLIST | /usr/bin/fgrep -v ${WHITELIST[@]/#/-e } > ./dnsmasq.blacklist.txt
