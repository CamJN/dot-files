#!/bin/bash
ruby -e "$(\curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install bash bash-completion bash-git-prompt git archey mysql postgresql rust wget foremost
brew install emacs --cocoa --srgb
brew install rbenv ruby-build passenger rbenv-default-gems rbenv-gem-rehash
brew install php54-mcrypt --without-homebrew-php
brew pin emacs
brew pin php54-mcrypt
