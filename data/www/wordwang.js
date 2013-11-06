function applyHandlers(handlers, x) {
    var i;
    for (i = 0; i < handlers.length; i++) {
        handlers[i](x);
    }
};

var ww = {
    debug: true,
    host: 'ws://localhost:8000/ws',

    // -----------------------------------------------------------------
    // Utils

    getHash: function() {
        var hash = window.location.hash.substring(1);
        if (hash === '') {
            hash = null;
        }
        return hash;
    },

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

function WWState(host, onopen) {
    var st = this;
    st.sock = new WebSocket(ww.host);
    if (onopen !== null) {
        st.sock.onopen = onopen;
    }

    // Main handler setup
    st.sock.onmessage = function(event) {
        ww.debugLog("received `" + event.data + "'");
        var resp = JSON.parse(event.data);
        var tag = resp.tag;
        if (!(tag in st._onRespHandlers)) {
            st._onRespHandlers[tag] = [];
        }
        applyHandlers(st._onRespHandlers[tag], resp);
        applyHandlers(st._onRespGlobalHandlers, resp);
    };
    st.sock.onclose = function(event) {
        ww.debugLog("connection closed!");
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
        st.story.blocks.push(resp.block);
    });
    st.onResp('candidate', function(resp) {
        st.story.candidates[resp.body.user] = resp.body;
    });
    st.onResp('vote', function(resp) {
        var votes = st.story.candidates[resp.candidate].votes;
        // TODO Should I check here?
        if (!(resp.vote in votes)) {
            votes.push(resp.vote);
        }
    });
    st.onResp('user', function(resp) {
        var users = st.story.users;
        if (!(resp.user in users)) {
            users.push(resp.user);
        }
    });
}

WWState.prototype = {
    sock: null,
    storyId: null,
    story: null,
    user: null,
    _onRespHandlers: {},
    _onRespGlobalHandlers: [],
    _lastReq: null,
    _lastResp: null,

    // -------------------------------------------------------------
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
        this._lastReq = req;
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

    vote: function(user) {
        this.sendReq('vote', {user: user});
    },

    closeVoting: function() {
        this.sendReq('closeVoting', {});
    },

    getStory: function() {
        this.sendReq('story', {});
    },

    // -------------------------------------------------------------
    // Events

    onOpen: function(f) {
        this._onOpenHandlers.push(f);
    },

    onResp: function(tag, f) {
        if (tag === null) {
            this._onRespGlobalHandlers.push(f);
        } else {
            if (!(tag in this._onRespHandlers)) {
                this._onRespHandlers[tag] = [];
            }
            this._onRespHandlers[tag].push(f);
        }
    }
};
