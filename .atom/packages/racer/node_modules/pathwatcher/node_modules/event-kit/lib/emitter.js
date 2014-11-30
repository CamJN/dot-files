(function() {
  var Disposable, Emitter;

  Disposable = require('./disposable');

  module.exports = Emitter = (function() {
    Emitter.prototype.isDisposed = false;

    function Emitter() {
      this.handlersByEventName = {};
    }

    Emitter.prototype.on = function(eventName, handler) {
      var currentHandlers;
      if (this.isDisposed) {
        throw new Error("Emitter has been disposed");
      }
      if (typeof handler !== 'function') {
        throw new Error("Handler must be a function");
      }
      if (currentHandlers = this.handlersByEventName[eventName]) {
        this.handlersByEventName[eventName] = currentHandlers.concat(handler);
      } else {
        this.handlersByEventName[eventName] = [handler];
      }
      return new Disposable(this.off.bind(this, eventName, handler));
    };

    Emitter.prototype.emit = function(eventName, value) {
      var handler, handlers, _i, _len, _ref, _results;
      if (handlers = (_ref = this.handlersByEventName) != null ? _ref[eventName] : void 0) {
        _results = [];
        for (_i = 0, _len = handlers.length; _i < _len; _i++) {
          handler = handlers[_i];
          _results.push(handler(value));
        }
        return _results;
      }
    };

    Emitter.prototype.dispose = function() {
      this.handlersByEventName = null;
      return this.isDisposed = true;
    };

    Emitter.prototype.off = function(eventName, handlerToRemove) {
      var handler, newHandlers, oldHandlers, _i, _len;
      if (this.isDisposed) {
        return;
      }
      if (oldHandlers = this.handlersByEventName[eventName]) {
        newHandlers = [];
        for (_i = 0, _len = oldHandlers.length; _i < _len; _i++) {
          handler = oldHandlers[_i];
          if (handler !== handlerToRemove) {
            newHandlers.push(handler);
          }
        }
        return this.handlersByEventName[eventName] = newHandlers;
      }
    };

    return Emitter;

  })();

}).call(this);
