
BrowserOpen = require "./browser-open"

module.exports =
  config:
    saveFilesBeforeRefresh:
      type: 'boolean'
      default: false
    chromeBackgroundRefresh:
      type: 'boolean'
      default: true
    googleChromeCanary:
      type: 'boolean'
      default: false
    googleChrome:
      type: 'boolean'
      default: true
    firefox:
      type: 'boolean'
      default: false
    safari:
      type: 'boolean'
      default: false
    firefoxNightly:
      type: 'boolean'
      default: false
    firefoxDeveloperEdition:
      type: 'boolean'
      default: false

  activate: (state) ->
    atom.commands.add 'atom-workspace', 'browser-refresh:open': ->
      BrowserOpen()

  deactivate: ->

  serialize: ->
