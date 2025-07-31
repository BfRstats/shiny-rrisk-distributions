// SwalService: to allow a queue of swal modals to work
// Taken from https://github.com/t4t5/sweetalert/issues/457
let SwalService = function(options) {
  this.pendingSwal = [];
  if (options && options.showPendingMessage !== undefined) {
    this.showPendingMessage = options.showPendingMessage;
  }
  this.initialize();
};

SwalService.prototype = {
  currentSwal: null,
  showPendingMessage: true,
  swalFirstCalled: false,
  __swal: swal,
  pendingSwal: null,
  nextId: 1,

  swal: function() {
    let pending = {
      args: arguments,
      id: this.nextId++
    };
    return this._swalWithId(pending);
  },

  _swalWithId: function(pending) {
    let service = this;
    if (this.isSwalOpen() || this.isClosing) {
      this.pendingSwal.push(pending);
    } else {
      this.__swal.apply(null, pending.args);
      this.currentSwal = pending;
      if (!this.swalFirstCalled) {
        $('.sweet-alert').prepend('<span class="other-messages"></span>');
        this.swalFirstCalled = true;
      }
    }
    this.refreshPendingText();
    return pending.id;
  },

  onClosed: function() {
    this.isClosing = false;
    this.openNextSwal();
  },

  refreshPendingText: function() {
    if (this.showPendingMessage) {
      if (this.pendingSwal.length === 0) {
        $('.other-messages').text('');
      } else {
        $('.other-messages').text(this.pendingSwal.length + ' unread alerts');
      }
    }
  },

  close: function(id) {
    if (id === undefined || (this.currentSwal && this.currentSwal.id == id)) {
      this.__swal.close();
    } else if (id !== undefined && this.pendingSwal.length > 0) {
      let indexOfSwalToClose;
      for (let i = 0; i < this.pendingSwal.length; i++) {
        if (this.pendingSwal[i].id == id) {
          indexOfSwalToClose = i;
          break;
        }
      }
      if (indexOfSwalToClose !== undefined) {
        this.pendingSwal.splice(indexOfSwalToClose, 1);
        this.refreshPendingText();
      }
    }
  },

  openNextSwal: function() {
    let service = this;
    if (service.pendingSwal.length > 0) {
      let pending = service.pendingSwal.shift();
      service._swalWithId(pending);
    }
  },

  isSwalOpen: function() {
    return this.currentSwal !== null && this.currentSwal !== undefined;
  },

  closeAndFireCallback: function(id, opts) {
    const _currentSwal = this.currentSwal;
    this.close(id);
    if (_currentSwal && 
        _currentSwal.args && 
        _currentSwal.args.length > 1 && 
        typeof _currentSwal.args[1] == 'function') {
      // Currently, programatically closing the swal doesn't invoke the callback.
      let callback = _currentSwal.args[1];
      callback(opts);
    }
  },

  initialize: function() {
    let service = this;
    let originalClose = this.__swal.close;
    this.__swal.close = function() {
      service.isClosing = true;
      originalClose();
      service.currentSwal = null;
      setTimeout(function() {
        service.onClosed();
      }, 400);
    };
  }
};
