var ww;
var wwSt;

(function() {
'use strict';

if (typeof Object.create !== 'function') {
    Object.create = function (o) {
        function F() {}
        F.prototype = o;
        return new F();
    };
}

var applyHandlers = function(handlers, x) {
    var i;
    for (i = 0; i < handlers.length; i++) {
        handlers[i](x);
    }
};

ww = {
    debug: true,
    host: 'ws://localhost:8000/ws',

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

    candidateForm: function() {
        return document.getElementById('candidateForm');
    },

    candidateBody: function() {
        return document.getElementById('candidateBody');
    },

    candidates: function() {
        return document.getElementById('candidates');
    },

    candidateEl: function(block) {
        var span = document.createElement('span');
        span.className = 'candidate';
        span.appendChild(document.createTextNode(block));
        return span;
    },

    // -----------------------------------------------------------------
    // Startup

    startup: function() {
        wwSt = WWState.new(ww.host);

        // Join on room creation
        wwSt.onResp('created', function(_) {
            ww.createDiv().style.display = 'none';
            ww.storyDiv().style.display = 'block';
            wwSt.join();
            window.location = ww.storyUrl(wwSt.storyId);
        });

        // Add listener to add candidates
        wwSt.onResp('candidate', function(resp) {
            var el = ww.candidateEl(resp.body.block);
            ww.candidates().appendChild(el);
        });

        // Add the listener to create stories when the button is
        // pressed
        ww.createForm().addEventListener('submit', function() {
            wwSt.create();
        });

        // Add the listener to submit candidates
        ww.candidateForm().addEventListener('submit', function() {
            wwSt.candidate(ww.candidateBody().value);
        });

        // Join existing story, or create it
        wwSt.onOpen(function(_) {
            var story = window.location.hash.substring(1);
            if (story !== '') {
                ww.storyDiv().style.display = 'block';
                wwSt.storyId = story;
                wwSt.join();
            } else {
                ww.createDiv().style.display = 'block';
            }
        });
    },

    // -----------------------------------------------------------------
    // Utils

    storyUrl: function(story) {
        var url = window.location.href.split('#')[0];
        return window.location = url + '#' + story;
    },

    debugLog: function(msg) {
        if (ww.debug) {
            console.log('[DEBUG] ' + msg);
        }
    },

    errorLog: function(msg) {
        console.log('[ERROR] ' + msg);
    }
};

var WWState = {
    sock: null,
    storyId: null,
    story: null,
    user: null,
    _onRespHandlers: {},
    _onRespGlobalHandlers: [],
    _onOpenHandlers: [],

    // -----------------------------------------------------------------
    // Requests

    sendReq: function(tag, body) {
        body.tag = tag;
        var req = {
            story: this.storyId,
            auth : null,
            body : body
        };
        if (this.user !== null) {
            req.auth = {user: this.user.id, secret: this.user.secret};
        }
        var payload = JSON.stringify(req);
        ww.debugLog('sending `' + payload + "'");
        this.sock.send(payload);
    },

    create: function() {
        if (this.storyId === null) {
            this.sendReq('create', {});
        } else {
            ww.debugLog("`WWState.create' but story already exists in state, ignoring");
        }
    },

    join: function() {
        if (this.user === null) {
            this.sendReq('join', {});
        } else {
            ww.debugLog("`WWState.join' but user already exists in state, ignoring");
        }
    },

    candidate: function(block) {
        this.sendReq('candidate', {block: block});
    },

    vote: function(st, user) {
        this.sendReq('vote', {user: user});
    },

    closeVoting: function(st) {
        this.sendReq('closeVoting', {});
    },

    // -----------------------------------------------------------------
    // Events

    onOpen: function(f) {
        this._onOpenHandlers.push(f);
    },

    onResp: function(tag, f) {
        var handlers;
        if (tag === null) {
            handlers = this._onRespGlobalHandlers;
        } else {
            if (!(tag in this._onRespHandlers)) {
                this._onRespHandlers[tag] = [];
            }
            handlers = this._onRespHandlers[tag];
        }
        handlers.push(f);
    },

    // -----------------------------------------------------------------
    // Static methods

    new: function(host) {
        var st = Object.create(WWState);
        st.sock = new WebSocket(ww.host);

        // Main handler setup
        st.sock.onmessage = function(event) {
            ww.debugLog("received `" + event.data + "'");
            var resp = JSON.parse(event.data);
            applyHandlers(st._onRespGlobalHandlers, resp);
            var tag = resp.tag;
            if (!(tag in st._onRespHandlers)) {
                st._onRespHandlers[tag] = [];
            }
            applyHandlers(st._onRespHandlers[tag], resp);
        };
        st.sock.onopen = function(event) {
            applyHandlers(st._onOpenHandlers, event);
        };

        // Base handlers
        st.onResp('joined', function(resp) {
            st.user = {id: resp.user, secret: resp.secret};
        });
        st.onResp('created', function(resp) {
            st.storyId = resp.story;
        });
        st.onResp('story', function(resp) {
            st.story = resp.body;
        });
        st.onResp('votingClosed', function(resp) {
            st.story.candidates = {},
            st.story.blocks.unshift(resp.block);
        });
        st.onResp('candidate', function(resp) {
            st.story.candidates[resp.body.user] = resp.body;
        });
        st.onResp('vote', function(resp) {
            var votes = st.story.candidates[resp.user].votes;
            // TODO Should I check here?
            if (!(resp.vote in votes)) {
                votes.push(resp.vote);
            }
        });
        return st;
    }
};

})();
