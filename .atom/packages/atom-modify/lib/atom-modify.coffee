module.exports =
  activate: (state) ->
    atom.workspaceView.command "atom-modify:convert", => @convert()

  convert: ->
    selection = atom.workspace.getActiveEditor().getSelection()
    chars = selection.getText().split('')

    @replaceChars chars, (final) ->
      final = final.join('')
      selection.insertText(final)

  # Iterate through characters, replacing non-html friendly entities
  replaceChars: (chars, callback) ->
    for char, i in chars
      if char of @exceptions
        chars[i] = @exceptions[char]
      else if char.charCodeAt(0) > 128
        chars[i] = '&#' + char.charCodeAt(0) + ';'

      if i is chars.length - 1
        callback(chars)

  # Custom replacements
  exceptions:
    '<' : '&lt;'
    '>' : '&gt;'
    '®' : '&reg;'
    '™' : '&trade;'
    '©' : '&copy;'
