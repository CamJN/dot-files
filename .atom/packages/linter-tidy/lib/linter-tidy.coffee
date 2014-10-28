linterPath = atom.packages.getLoadedPackage("linter").path
Linter = require "#{linterPath}/lib/linter"

class LinterTidy extends Linter
  # The syntax that the linter handles. May be a string or
  # list/tuple of strings. Names should be all lowercase.
  @syntax: ['text.html.basic']

  # A string, list, tuple or callable that returns a string, list or tuple,
  # containing the command line (with arguments) used to lint.
  cmd: 'tidy -quiet -utf8'

  executablePath: null

  linterName: 'tidy'

  errorStream: 'stderr'

  # A regex pattern used to extract information from the executable's output.
  regex: 'line (?<line>\\d+) column (?<col>\\d+) - ((?<error>Error)|(?<warning>Warning)): (?<message>.+)'

  constructor: (editor) ->
    super(editor)

    atom.config.observe 'linter-tidy.tidyExecutablePath', =>
      @executablePath = atom.config.get 'linter-tidy.tidyExecutablePath'

  destroy: ->
    atom.config.unobserve 'linter-tidy.tidyExecutablePath'

  formatMessage: (match) ->
    @escapeMessage(match.message)

  escapeMessage: (message) ->
    message.replace(/[&"'<>]/g, @escapeMessageReplace)

  escapeMessageReplace: (match) ->
    switch match
      when '&' then '&amp;'
      when '"' then '&quot;'
      when "'" then '&#39;'
      when '<' then '&lt;'
      when '>' then '&gt;'
      else match

module.exports = LinterTidy
