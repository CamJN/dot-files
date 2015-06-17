{BufferedProcess} = require 'atom'
OS = require 'os'
StatusView = require './status-view'

MacChromeActivation = atom.config.get 'browser-refresh.chromeBackgroundRefresh'
if (MacChromeActivation == false)
  MacChromeActivation = "activate"
else
  MacChromeActivation = ""

MacChromeCmd = """
tell application "Google Chrome"
  #{MacChromeActivation}
  "chrome"
  set winref to a reference to (first window whose title does not start with "Developer Tools - ")
  set winref's index to 1
  reload active tab of winref
end tell
"""
MacChromeCanaryCmd = """
tell application "Google Chrome Canary"
  #{MacChromeActivation}
  "chrome canary"
  set winref to a reference to (first window whose title does not start with "Developer Tools - ")
  set winref's index to 1
  reload active tab of winref
end tell
"""

MacFirefoxCmd = """
set a to path to frontmost application as text
tell application "Firefox"
	activate
	tell application "System Events" to keystroke "r" using command down
end tell
delay 0.2
activate application a
"""

MacFirefoxNightlyCmd = """
set a to path to frontmost application as text
tell application "FirefoxNightly"
	activate
	tell application "System Events" to keystroke "r" using command down
end tell
delay 0.2
activate application a
"""

MacFirefoxDeveloperEditionCmd = """
set a to path to frontmost application as text
tell application "FirefoxDeveloperEdition"
	activate
	tell application "System Events" to keystroke "r" using command down
end tell
delay 0.2
activate application a
"""

MacSafariCmd = """
tell application "Safari"
  activate
  tell its first document
    set its URL to (get its URL)
  end tell
end tell
"""

Commands = {
  darwin: {
    firefox      : MacFirefoxCmd,
    firefoxNightly : MacFirefoxNightlyCmd,
    firefoxDeveloperEdition : MacFirefoxDeveloperEditionCmd,
    chrome       : MacChromeCmd,
    chromeCanary : MacChromeCanaryCmd,
    safari       : MacSafariCmd
  },
  linux: {
    firefox: [
      'search',
      '--sync',
      '--onlyvisible',
      '--class',
      'firefox',
      'key',
      'F5',
      'windowactivate'
    ],
    chrome: [
      'search',
      '--sync',
      '--onlyvisible',
      '--class',
      'chrome',
      'key',
      'F5',
      'windowactivate'
    ]
  }
}

OpenPanel = (params) ->
  statusView = new StatusView(params)
  statusPanel = atom.workspace.addBottomPanel(item: statusView)
  setTimeout ->
    statusView?.destroy()
    statusView = null
    statusPanel?.destroy()
    statusPanel = null
  , 2000

RunMacCmd = (BrowserCmd) ->
  new BufferedProcess({
    command: 'osascript'
    args: ['-e', BrowserCmd]
    stderr: (data) ->
      OpenPanel(type: 'alert', message: data.toString())
  })

RunLinuxCmd = (BrowserArgs) ->
  new BufferedProcess({
    command: 'xdotool'
    args: BrowserArgs
    stderr: (data) ->
      OpenPanel(type: 'alert', message: data.toString())
  })

RunCmd = (browser) ->
  if OS.platform() == 'darwin'
    RunMacCmd(Commands['darwin'][browser])
  else if OS.platform() == 'linux' and browser isnt 'safari'
    RunLinuxCmd(Commands['linux'][browser])
  else
    OpenPanel(type: 'alert', message: 'Unsupported platform')

BrowserOpen = ()->
  if(atom.config.get 'browser-refresh.saveCurrentFileBeforeRefresh')
    atom.workspace.getActiveEditor().save()
  if(atom.config.get 'browser-refresh.saveFilesBeforeRefresh')
    atom.workspace.saveAll()
  if(atom.config.get 'browser-refresh.firefox')
    RunCmd('firefox')
  if(atom.config.get 'browser-refresh.firefoxNightly')
    RunCmd('firefoxNightly')
  if(atom.config.get 'browser-refresh.firefoxDeveloperEdition')
    RunCmd('firefoxDeveloperEdition')
  if(atom.config.get 'browser-refresh.googleChrome')
    RunCmd('chrome')
  if(atom.config.get 'browser-refresh.googleChromeCanary')
    RunCmd('chromeCanary')
  if(atom.config.get 'browser-refresh.safari')
    RunCmd('safari')

module.exports = BrowserOpen
