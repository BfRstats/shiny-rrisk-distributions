let swalService = new SwalService({showPendingMessage: false});
shinyalert = {};
shinyalert.instances = [];

Shiny.addCustomMessageHandler(
  'shinyalert.show', function(params) 
  {
    Shiny.unbindAll($(".sweet-alert"));
    
    let callbackJS = function(value) {};
    if (params.callbackJS !== null) {
      const cb = params.callbackJS;
      callbackJS = function(value) { eval("("+cb+")(value)") };
      delete params.callbackJS;
    }
    
    let callbackR = function(value) {};
    const cbid = params.cbid;
    delete params.cbid;
    if (params.callbackR) {
      callbackR = function(value) {
        Shiny.onInputChange(cbid, value);
      };
    }
    
    let callback = function(value) {
      for (let idx in shinyalert.instances) {
        if (shinyalert.instances[idx].cbid === cbid) {
          shinyalert.instances.splice(idx, 1);
        }
        break;
      }
      
      // Avoid duplicated callback calls
      if (typeof params.inputId === 'string') {
        if ('compareVersion' in Shiny &&
            Shiny.compareVersion(Shiny.version, ">=", "1.1.0") ) {
          Shiny.setInputValue(params.inputId, value, {priority: "event"});
        } else {
          Shiny.onInputChange(params.inputId, value);
        }
        callbackJS(value);
        callbackR(value);
        delete params.inputId;
      }
    };
    
    const timer = params.timer;
    delete params.timer;
    
    const swal_id = swalService.swal(params, callback);
    shinyalert.instances.push({
      swal_id : swal_id,
      cbid    : cbid
    });
    
    if (timer > 0) {
      setTimeout(
        function(x) {
          let alertidx = 0;
          for (alertidx in shinyalert.instances) {
            if (shinyalert.instances[alertidx].swal_id === x) {
              shinyalert.instances.splice(alertidx, 1);
            }
            swalService.closeAndFireCallback(x, false);
            break;
          }
        }, 
        timer, swal_id
      );
    }
    
    // Enable MathJax
    if (window.MathJax !== undefined) {
      MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    }
    
    // Modals that need a scrollbar get initialized at the bottom, so let's scroll to top
    $('.sweet-alert')[0].scrollTop = 0;
});

Shiny.addCustomMessageHandler(
  'shinyalert.closeAlert', function(params) 
  {
    const cbid = params.cbid;
    
    if (typeof cbid === 'string') {
      // close a specific alert
      for (let idx = 0; idx < shinyalert.instances.length; idx++ ){
        const item = shinyalert.instances[idx];
        if (item.cbid === cbid) {
          shinyalert.instances.splice(idx, 1);
          swalService.closeAndFireCallback(item.swal_id, false);
          break;
        }
      }
    } else {
      // close n alerts
      const num = params.count || shinyalert.instances.length;
      const items = shinyalert.instances.splice(0, num);
      for (let idx = 0; idx < items.length; idx++) {
        swalService.closeAndFireCallback(items[idx].swal_id, false);
      }
    }
  }
);
