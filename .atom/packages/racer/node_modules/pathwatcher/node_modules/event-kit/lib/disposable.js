(function() {
  var Disposable, Grim;

  Grim = require('grim');

  module.exports = Disposable = (function() {
    Disposable.prototype.isDisposed = false;

    function Disposable(disposalAction) {
      this.disposalAction = disposalAction;
    }

    Disposable.prototype.dispose = function() {
      if (!this.isDisposed) {
        this.isDisposed = true;
        return typeof this.disposalAction === "function" ? this.disposalAction() : void 0;
      }
    };

    Disposable.prototype.off = function() {
      Grim.deprecate("Use ::dispose to cancel subscriptions instead of ::off");
      return this.dispose();
    };

    return Disposable;

  })();

}).call(this);
