#!/bin/bash
ruby -e "$(\curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor
brew tap homebrew/dupes homebrew/php
brew install archey awscli bash bash-completion bash-git-prompt dnsmasq foremost git imagemagick mysql node passenger postgresql qt5 rbenv-default-gems rbenv-gem-rehash tidy-html5 unrar wget rust
ln -s  /Users/camdennarzt/Developer/Bash/dot-files/homebrew/racer.rb /usr/local/Library/Formula/racer
brew install racer
brew install php56 --with-postgresql
brew install php56-xhprof php56-mcrypt
brew install emacs --cocoa
brew pin emacs
brew pin php56

#links:
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/dnsmasq.conf /usr/local/etc/dnsmasq.conf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/openssl/openssl.cnf /usr/local/etc/openssl/openssl.cnf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/php/php.ini /usr/local/etc/php/5.6/php.ini
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-languages.conf /private/etc/apache2/extra/httpd-languages.conf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-ssl.conf /private/etc/apache2/extra/httpd-ssl.conf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/extra/httpd-vhosts.conf /private/etc/apache2/extra/httpd-vhosts.conf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/httpd.conf /private/etc/apache2/httpd.conf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/apache/other /private/etc/apache2/other
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/hosts /private/etc/hosts
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/postfix/main.cf /private/etc/postfix/main.cf
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/resolver /private/etc/resolver
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/shells /private/etc/shells
ln -s /Users/camdennarzt/Developer/Bash/dot-files/etc/mysql/my.cnf /usr/local/etc/my.cnf
#ln -s /Users/camdennarzt/Developer/Bash/dot-files/______    /usr/local/var/postgres/postgresql.conf
