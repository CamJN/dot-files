(function() {
  var CompositeDisposable;

  module.exports = CompositeDisposable = (function() {
    CompositeDisposable.prototype.isDisposed = false;

    function CompositeDisposable() {
      this.disposables = [];
    }

    CompositeDisposable.prototype.add = function(disposable) {
      if (!this.isDisposed) {
        return this.disposables.push(disposable);
      }
    };

    CompositeDisposable.prototype.remove = function(disposable) {
      var index;
      index = this.disposables.indexOf(disposable);
      if (index !== -1) {
        return this.disposables.splice(index, 1);
      }
    };

    CompositeDisposable.prototype.dispose = function() {
      var disposable, _i, _len, _ref;
      if (!this.isDisposed) {
        _ref = this.disposables;
        for (_i = 0, _len = _ref.length; _i < _len; _i++) {
          disposable = _ref[_i];
          disposable.dispose();
        }
        return this.clear();
      }
    };

    CompositeDisposable.prototype.clear = function() {
      return this.disposables.length = 0;
    };

    return CompositeDisposable;

  })();

}).call(this);
