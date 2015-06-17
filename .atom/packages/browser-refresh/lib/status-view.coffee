{View} = require 'atom-space-pen-views'

module.exports =
  class StatusView extends View
    @content = (params) ->
      @div class: 'browser-refresh-view', =>
        @div class: "type-#{params.type}", 'Browser Refresh: ' + params.message

    destroy: ->
