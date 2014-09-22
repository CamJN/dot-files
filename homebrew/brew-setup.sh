#!/bin/bash
ruby -e "$(\curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install bash bash-completion bash-git-prompt git mysql postgresql rust wget rbenv ruby-build passenger
# brew install emacs r-gui sqlite3 inkscape
brew install php54-mcrypt --without-homebrew-php
