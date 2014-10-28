linterPath = atom.packages.getLoadedPackage("linter").path
Linter = require "#{linterPath}/lib/linter"
{Range, Point, BufferedProcess} = require 'atom'
{log, warn} = require "#{linterPath}/lib/utils"

class LinterErb extends Linter
  # The syntax that the linter handles. May be a string or
  # list/tuple of strings. Names should be all lowercase.
  @syntax: ['text.html.erb']

  # A string, list, tuple or callable that returns a string, list or tuple,
  # containing the command line (with arguments) used to lint.
  rubyCmd: 'ruby -c -'
  erbCmd: 'erb -x -T -'

  executablePath: null

  linterName: 'erb'

  # A regex pattern used to extract information from the executable's output.
  regex: '.+:(?<line>\\d+):(?<error>)(?<message>.+)'

  constructor: (editor) ->
    super(editor)

    atom.config.observe 'linter-erb.erbExecutablePath', =>
      @executablePath = atom.config.get 'linter-erb.erbExecutablePath'

  destroy: ->
    atom.config.unobserve 'linter-erb.erbExecutablePath'

  lintFile: (filePath, callback) ->
    # build the command with arguments to lint the file
    @cmd = @rubyCmd
    {command: rubyCommand, args: rubyArgs} = @getCmdAndArgs(null)
    @cmd = @erbCmd
    {command: erbCommand, args: erbArgs} = @getCmdAndArgs(filePath)

    # options for BufferedProcess, same syntax with child_process.spawn
    rubyOptions = {stdio: ['pipe', null, null]}
    erbOptions = {cwd: @cwd}

    data = []

    rubyStderr = (output) ->
      warn 'stderr', output
      data += output

    rubyExit = =>
      @processMessage data, callback

    erbStdout = (output) ->
      log 'stdout', output
      rubyProcess.process.stdin.write(output)

    erbExit = =>
      rubyProcess.process.stdin.end()

    rubyProcess = new BufferedProcess({command: rubyCommand, args: rubyArgs
                                     , options: rubyOptions, stderr: rubyStderr
                                     , exit: rubyExit})

    erbProcess = new BufferedProcess({command: erbCommand, args: erbArgs
                                    , options: erbOptions, stdout: erbStdout
                                    , exit: erbExit})

    # Don't block UI more than 5seconds, it's really annoying on big files
    setTimeout ->
      rubyProcess.kill()
      erbProcess.kill()
    , 5000

  createMessage: (match) ->
    # Easy fix when editor has no newline at end of file
    if match.line > @editor.getLineCount()
      match.line = @editor.getLineCount()
    super(match)

module.exports = LinterErb
