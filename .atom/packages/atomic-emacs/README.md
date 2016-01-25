## Atomic Emacs

Emacs keybindings for Atom.
![Build Status](https://travis-ci.org/avendael/atomic-emacs.svg?branch=master)

## Commands

### Navigation

    'ctrl-b': 'atomic-emacs:backward-char'
    'left': 'atomic-emacs:backward-char'
    'ctrl-f': 'atomic-emacs:forward-char'
    'right': 'atomic-emacs:forward-char'
    'alt-b': 'atomic-emacs:backward-word'
    'alt-left': 'atomic-emacs:backward-word'
    'alt-f': 'atomic-emacs:forward-word'
    'alt-right': 'atomic-emacs:forward-word'
    'ctrl-alt-b': 'atomic-emacs:backward-sexp'
    'ctrl-alt-f': 'atomic-emacs:forward-sexp'
    'alt-{': 'atomic-emacs:backward-paragraph'
    'alt-}': 'atomic-emacs:forward-paragraph'
    'alt-m': 'atomic-emacs:back-to-indentation'
    'ctrl-a': 'editor:move-to-beginning-of-line'
    'ctrl-s': 'find-and-replace:show'
    'ctrl-r': 'find-and-replace:show'
    'alt-<': 'core:move-to-top'
    'alt->': 'core:move-to-bottom'

### Killing & Yanking

    'alt-backspace': 'atomic-emacs:backward-kill-word'
    'alt-delete': 'atomic-emacs:backward-kill-word'
    'alt-d': 'atomic-emacs:kill-word'
    'ctrl-k': 'atomic-emacs:kill-line'
    'ctrl-w': 'atomic-emacs:kill-region'
    'alt-w': 'atomic-emacs:copy-region-as-kill'
    'ctrl-alt-w': 'atomic-emacs:append-next-kill'
    'ctrl-y': 'atomic-emacs:yank'
    'alt-y': 'atomic-emacs:yank-pop'
    'alt-shift-y': 'atomic-emacs:yank-shift'

Note that Atomic Emacs does not (yet) support prefix arguments, so to rotate the
kill ring forward, use `yank-shift` (equivalent to `yank-pop` in Emacs with a
prefix argument of -1).

### Editing

    'alt-\\': 'atomic-emacs:delete-horizontal-space'
    'alt-^': 'atomic-emacs:delete-indentation'
    'ctrl-o': 'atomic-emacs:open-line'
    'alt-space': 'atomic-emacs:just-one-space'
    'ctrl-t': 'atomic-emacs:transpose-chars'
    'alt-t': 'atomic-emacs:transpose-words'
    'ctrl-x ctrl-t': 'atomic-emacs:transpose-lines'
    'ctrl-x ctrl-l': 'atomic-emacs:downcase-word-or-region'
    'alt-l': 'atomic-emacs:downcase-word-or-region'
    'ctrl-x ctrl-u': 'atomic-emacs:upcase-word-or-region'
    'alt-u': 'atomic-emacs:upcase-word-or-region'
    'alt-c': 'atomic-emacs:capitalize-word-or-region'
    'ctrl-j': 'editor:newline'
    'ctrl-/': 'core:undo'
    'ctrl-_': 'core:undo'
    'alt-/': 'autocomplete-plus:activate'
    'alt-q': 'autoflow:reflow-selection'
    'alt-;': 'editor:toggle-line-comments'

### Marking & Selecting

    'ctrl-space': 'atomic-emacs:set-mark'
    'ctrl-alt-space': 'atomic-emacs:mark-sexp'
    'ctrl-x h': 'atomic-emacs:mark-whole-buffer'
    'ctrl-x ctrl-x': 'atomic-emacs:exchange-point-and-mark'

### UI

    'ctrl-g': 'core:cancel'
    'ctrl-x ctrl-s': 'core:save'
    'alt-x': 'command-palette:toggle'
    'alt-.': 'symbols-view:toggle-file-symbols'
    'ctrl-x ctrl-f': 'fuzzy-finder:toggle-file-finder'
    'ctrl-x b': 'fuzzy-finder:toggle-buffer-finder'
    'ctrl-x k': 'core:close'
    'ctrl-x 0': 'pane:close'
    'ctrl-x 1': 'atomic-emacs:close-other-panes'
    'ctrl-x 2': 'pane:split-down'
    'ctrl-x 3': 'pane:split-right'
    'ctrl-x o': 'window:focus-next-pane'

### Something missing?

Feel free to suggest features on the Github issue tracker, or better yet, send a
pull request!

## Contributing

* [Bug reports](https://github.com/avendael/atomic-emacs/issues)
* [Source](https://github.com/avendael/atomic-emacs)
* Patches: Fork on Github, send pull request.
 * Include tests where practical.
 * Leave the version alone, or bump it in a separate commit.
