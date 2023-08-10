#!/usr/local/bin/bash

for file in $HOME/.emacs.d/elpa/tree-sitter-langs-*/bin/*.dylib; do
    ln -sf $file $HOME/.emacs.d/tree-sitter/libtree-sitter-$(basename $file)
done
