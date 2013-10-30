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
    debug: true,
    host: 'ws://localhost:8000/ws',
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
            ww.debugLog("wordwang: received `" + event.data + "'");
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
    
    // -----------------------------------------------------------------
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
        ww.debugLog('wordwang: sending `' + payload + "'");
        st.sock.send(payload);
    },

    create: function(st) {
        if (st.story === null) {
            ww.sendReq(st, 'create', {});
        } else {
            ww.debugLog("`ww.create' but story already exists in state, ignoring");
        }
    },
    
    join: function(st) {
        if (st.user === null) {
            ww.sendReq(st, 'join', {});
        } else {
            ww.debugLog("`ww.join' but user already exists in state, ignoring");
        }
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
    
    // -----------------------------------------------------------------
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

    // -----------------------------------------------------------------
    // Elements

    createDiv: function() {
        return document.getElementById('create');
    },

    storyDiv: function() {
        return document.getElementById('story');
    },

    createForm: function() {
        return document.getElementById('createForm');
    },

    // -----------------------------------------------------------------
    // Startup

    startup: function() {
        window.wwSt = ww.newState(ww.host);

        // Join on room creation
        ww.onResp(window.wwSt, 'created', function(_) {
            ww.createDiv().style.display = 'none';
            ww.storyDiv().style.display = 'block';
            ww.join(window.wwSt);
        });

        // Add the listener to create stories when the button is
        // pressed
        ww.createForm().addEventListener('submit', function() {
            ww.create(window.wwSt);
        });

        ww.onOpen(window.wwSt, function(_) {
            var story = window.location.hash.substring(1);
            if (story !== '') {
                // Join existing one
                ww.storyDiv().style.display = 'block';
                window.wwSt.story = story;
                ww.join(window.wwSt);
            } else {
                // Create it
                ww.createDiv().style.display = 'block';
            }
        });
    },

    // -----------------------------------------------------------------
    // Utils

    debugLog: function(msg) {
        if (ww.debug) {
            console.log(msg);
        }
    },

    errorLog: function(msg) {
        if (ww.debug) {
            alert(msg);
        } else {
            console.log(msg);
        }
    }
};
})();
