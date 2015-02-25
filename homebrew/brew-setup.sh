#!/bin/bash
ruby -e "$(\curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor
brew tap homebrew/dupes homebrew/php
brew install archey bash bash-completion bash-git-prompt dnsmasq foremost git imagemagick mysql ossp-uuid passenger qt rbenv-default-gems rbenv-gem-rehash rust wget whereami
brew install tidy php56-mcrypt php56-pdo-pgsql php56 --with-homebrew-openssl --with-tidy --without-snmp --with-postgresql
brew install emacs --cocoa --srgb
brew pin emacs
brew pin php56

#links: ln -s
/usr/local/etc/dnsmasq.conf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/dnsmasq.conf
/usr/local/etc/openssl/openssl.cnf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/openssl/openssl.cnf
/usr/local/etc/php/5.6/php.ini -> /Users/camdennarzt/Developer/Bash/dot-files/etc/php/php.ini
/private/etc/apache2/extra/httpd-languages.conf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-languages.conf
/private/etc/apache2/extra/httpd-ssl.conf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-ssl.conf
/private/etc/apache2/extra/httpd-vhosts.conf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-vhosts.conf
/private/etc/apache2/httpd.conf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/httpd.conf
/private/etc/apache2/other -> /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/other
/private/etc/hosts -> /Users/camdennarzt/Developer/Bash/dot-files/etc/hosts
/private/etc/postfix/main.cf -> /Users/camdennarzt/Developer/Bash/dot-files/etc/postfix/main.cf
/private/etc/resolver -> /Users/camdennarzt/Developer/Bash/dot-files/etc/resolver
/private/etc/shells -> /Users/camdennarzt/Developer/Bash/dot-files/etc/shells
