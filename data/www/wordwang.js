var ww;

(function() {
 'use strict';

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
      _onOpenHandlers: [],

      newState: function(host) {
          var st = {
              sock: new WebSocket(host),
              story: null,
              user: null
          };
          st.sock.onmessage = function(event) {
              console.log("wordwang: received `" + event.data + "'");
              var resp = JSON.parse(event.data);
              applyHandlers(ww._onRespGlobalHandlers, resp);
              var tag = resp.tag;
              if (!(tag in ww._onRespHandlers)) {
                  ww._onRespHandlers[tag] = [];
              }
              applyHandlers(ww._onRespHandlers[tag], resp);
          };
          st.sock.onopen = function(event) {
              applyHandlers(ww._onOpenHandlers, event);
          };
          ww.onResp(st, 'created', function(resp) {
              st.story = resp.story;
          });
          ww.onResp(st, 'joined', function(resp) {
              st.user = {id: resp.user, secret: resp.secret};
          });
          return st;
      },

      // ---------------------------------------------------------------
      // Requests

      sendReq: function(st, tag, body) {
          body.tag = tag;
          var req = {
              story: st.story,
              auth : null,
              body : body
          };
          if (st.user !== null) {
              req.auth = {authUser: st.user.id, authSecret: st.user.secret};
          }
          var payload = JSON.stringify(req);
          console.log('wordwang: sending `' + payload + "'");
          st.sock.send(payload);
      },

      create: function(st) {
          ww.sendReq(st, 'create', {});
      },

      join: function(st) {
          ww.sendReq(st, 'join', {});
      },

      candidate: function(st, block) {
          ww.sendReq(st, 'candidate', {block: block});
      },

      vote: function(st, user) {
          ww.sendReq(st, 'vote', {user: user});
      },

      closeVoting: function(st) {
          ww.sendReq(st, 'closeVoting', {});
      },

      // ---------------------------------------------------------------
      // Events

      onOpen: function(st, f) {
          ww._onOpenHandlers.push(f);
      },

      onResp: function(st, tag, f) {
          var handlers;
          if (tag === null) {
              handlers = ww._onRespGlobalHandlers;
          } else {
              if (!(tag in ww._onRespHandlers)) {
                  ww._onRespHandlers[tag] = [];
              }
              handlers = ww._onRespHandlers[tag];
          }
          handlers.push(f);
      },

      // ---------------------------------------------------------------
      // Startup

      startup: function() {
          var story = window.location.hash.substring(1);
          var createDiv = document.getElementById('create');
          var storyDiv   = document.getElementById('story');
          if (story !== '') {
              storyDiv.style.display = 'block';
          } else {
              createDiv.style.display = 'block';
          }
      }
  };
})();
