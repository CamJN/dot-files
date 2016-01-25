'use babel';

import * as path from 'path';

describe('The ERB provider for Linter', () => {
  const lint = require(path.join('..', 'lib', 'index.js')).provideLinter().lint;

  beforeEach(() => {
    atom.workspace.destroyActivePaneItem();
    waitsForPromise(() => {
      atom.packages.activatePackage('linter-erb');
      return atom.packages.activatePackage('language-ruby').then(() =>
        atom.workspace.open(path.join(__dirname, 'fixtures', 'good.erb'))
      );
    });
  });

  describe('checks a file with issues and', () => {
    let editor = null;
    const badFile = path.join(__dirname, 'fixtures', 'bad.erb');
    beforeEach(() => {
      waitsForPromise(() => {
        return atom.workspace.open(badFile).then(openEditor => {
          editor = openEditor;
        });
      });
    });

    it('finds at least one message', () => {
      waitsForPromise(() => {
        return lint(editor).then(messages => {
          expect(messages.length).toBeGreaterThan(0);
        });
      });
    });

    it('verifies the first message', () => {
      waitsForPromise(() => {
        const messageText = 'unexpected keyword_in, expecting end-of-input';
        return lint(editor).then(messages => {
          expect(messages[0].type).toBeDefined();
          expect(messages[0].type).toEqual('Error');
          expect(messages[0].text).toBeDefined();
          expect(messages[0].text).toEqual(messageText);
          expect(messages[0].filePath).toBeDefined();
          expect(messages[0].filePath).toMatch(/.+bad\.erb$/);
          expect(messages[0].range).toBeDefined();
          expect(messages[0].range.length).toBeDefined();
          expect(messages[0].range.length).toEqual(2);
          expect(messages[0].range).toEqual([[0, 0], [0, 32]]);
        });
      });
    });
  });

  it('finds nothing wrong with a valid file', () => {
    waitsForPromise(() => {
      const goodFile = path.join(__dirname, 'fixtures', 'good.erb');
      return atom.workspace.open(goodFile).then(editor => {
        return lint(editor).then(messages => {
          expect(messages.length).toEqual(0);
        });
      });
    });
  });
});
