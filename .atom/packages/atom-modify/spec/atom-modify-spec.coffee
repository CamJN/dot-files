{WorkspaceView} = require 'atom'

describe "AtomModify", ->
  [activationPromise, editor, editorView] = []

  convert = (callback) ->
    editorView.trigger 'atom-modify:convert'
    waitsForPromise -> activationPromise
    runs(callback)
      
  beforeEach ->
    atom.workspaceView = new WorkspaceView
    atom.workspace = atom.workspaceView.model
    atom.workspaceView.openSync()
    
    editorView = atom.workspaceView.getActiveView()
    editor = editorView.getEditor()

    activationPromise = atom.packages.activatePackage('atom-modify')
    
  describe "when the atom-modify:convert event is triggered", ->
    it "converts a special character to html entity", ->
      editor.setText 'This is a dagger †'
      editor.selectAll()
      
      convert ->
        expect(editor.getText()).toBe 'This is a dagger &#8224;'

    it "converts our custom exceptions", ->
      editor.setText '<html>©®™'
      editor.selectAll()
      
      convert ->
        expect(editor.getText()).toBe '&lt;html&gt;&copy;&reg;&trade;'
