# linter-erb

This package will lint your ERB files in Atom by running them through `erb -x`
and sending the output to `ruby -c` for checking.

## Installation

### Ruby

Before using this package you will need to have [Ruby][] installed and
available from your `$PATH`.

### Plugin installation

The [Linter][] and package will be installed for you to provide an interface
to this package. If you are using an alternative debugging interface that
supports linter plugins simply disable [Linter][].

If you do not already have it installed, [language-ruby][] will also be
installed for you.

To install this packge either search for it from within Atom's settings menu
or run the following command.

```ShellSession
apm install linter-erb
```

## Settings

All of linter-erb's settings are available from within Atom's settings menu.
If you prefer to manually edit the configuration file the following settings
are available:

*   `erbExecutablePath`: Defaults to `erb`, allowing the `$PATH` to resolve the
    correct location. If you need to override this specify the full path to
    `erb`.

*   `rubyExecutablePath`: Defaults to `erb`, allowing the `$PATH` to resolve the
    correct location. If you need to override this specify the full path to
    `erb`.

*   `trimMode`: Select the trim mode used by ERB when generating the code
    before sending off to Ruby for checking. Defaults to no trimming. For a
    full description of what the options mean see [the documentation](https://docs.oracle.com/cd/E53394_01/html/E54763/erb-1.html).

    _**Note**_: Modes other than `None` _may_ cause the error line numbers to
    not match the source line numbers.

### Finding the full path

If you are unsure of the location on your system of a program there are
utilities you can use to find the full path. For example to find the path to
`erb` you would run one of the following:

*   On a UNIX / OS X system simply run `which erb` in a terminal. You should
    get something similar to `/usr/bin/erb`.

*   On Windows run `where.exe erb` in a prompt. You should get back something
    similar to `C:\Ruby22\bin\erb`.

[linter]: https://github.com/atom-community/linter "Linter"
[language-ruby]: https://github.com/atom/language-ruby "language-ruby"
[ruby]: http://www.ruby-lang.org/ "Ruby"
