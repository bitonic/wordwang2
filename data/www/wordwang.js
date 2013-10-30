var ww;

(function() {
 "use strict";

  var applyHandlers = function(handlers, x) {
      var i;
      for (i = 0; i < handlers.length; i++) {
          handlers[i](x);
      }
  };

  ww = {
      debug: false,
      _onRespHandlers: {},
      _onRespGlobalHandlers: [],

      sendReq: function(sock, obj) {
          var payload = JSON.stringify(obj);
          console.log("wordwang: sending `" + payload + "'");
          sock.send(payload);
      },

      onResp: function(sock, f, tag) {
          sock.onmessage = function(event) {
              console.log("wordwang: received `" + event.data + "'");
              var resp = JSON.parse(event.data);
              if (tag === undefined) {
                  applyHandlers(ww._onRespGlobalHandlers, resp);
              } else {
                  if (!(tag in ww._onRespHandlers)) {
                      ww._onRespHandlers[tag] = [];
                  }
                  applyHandlers(ww._onRespHandlers[tag], resp);
            }
          };
      },

      startup: function() {
          var room = window.location.hash.substring(1);
          if (room !== "") {
              console.log("will join room " + room);
          } else {
              console.log("will show login page");
          }
      }
  };
})();
