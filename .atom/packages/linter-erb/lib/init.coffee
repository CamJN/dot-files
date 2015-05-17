module.exports =
  config:
    erbExecutablePath:
      default: ''
      title: 'Erb Executable Path'
      type: 'string'

  activate: ->
    console.log 'activate linter-erb'
