'use babel'

const LOCAL_BIN_PATH = '/usr/local/bin'

export function getPath() {
  if (process.platform !== 'darwin') {
    return process.env.PATH
  }
  if (global.__STEELBRAIN_CONSISTENT_PATH) {
    return global.__STEELBRAIN_CONSISTENT_PATH
  }
  const shellPath = require('shell-path')
  // Line copied from https://github.com/sindresorhus/fix-path/blob/master/index.js
  let path = shellPath.sync() || [
      './node_modules/.bin',
      '/.nodebrew/current/bin',
      LOCAL_BIN_PATH,
      process.env.PATH
    ].join(':')
  if (path.indexOf(LOCAL_BIN_PATH) === -1) {
    path += ':' + LOCAL_BIN_PATH
  }
  global.__STEELBRAIN_CONSISTENT_PATH = path
  return path
}
