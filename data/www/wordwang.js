function applyHandlers(handlers, x) {
    var i;
    for (i = 0; i < handlers.length; i++) {
        handlers[i](x);
    }
};

var ww = {
    debug: true,
    host: 'ws://localhost:8888/ws',

    // -----------------------------------------------------------------
    // Utils

    getHash: function() {
        var hash = window.location.hash.substring(1);
        if (hash === '') {
            hash = null;
        }
        return hash;
    },

    roomUrl: function(story) {
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
    st.onResp('join', function(resp) {
        st.userId = resp.userId;
        st.user   = resp.user;
    });
    st.onResp('story', function(resp) {
        st.story = resp.body;
    });
    st.onResp('error', function(resp) {
        // TODO do something
    });
    st.onResp('patch', function(resp) {
        st.applyPatch(resp.body);
    });
}

WWState.prototype = {
    sock: null,
    roomId: null,
    verStory: null,
    userId: null,
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
            roomId: this.roomId,
            auth: null,
            body: body
        };
        if (this.user !== null) {
            req.auth = {userId: this.userId, secret: this.user.secret};
        }
        var payload = JSON.stringify(req);
        ww.debugLog('sending `' + payload + "'");
        this.sock.send(payload);
        this._lastReq = req;
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

    vote: function(candidateId) {
        this.sendReq('vote', {candidateId: candidateId});
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
    },

    // -------------------------------------------------------------
    // Patches

    applyPatch: function(patch) {
        var story = this.story;
        if (patch.tag === 'votingClosed') {
            story.candidates = {};
            story.blocks.push(patch.block);
        } else if (patch.tag === 'candidate') {
            story.candidates[patch.candidateId] = patch.candidate;
        } else if (patch.tag === 'vote') {
            var votes = story.candidates[patch.candidateId].votes;
            if (votes.indexOf(patch.userId) === -1) {
                votes.push(patch.userId);
            }
        } else {
            ww.errorLog('unrecognised patch ' + JSON.stringify(patch));
        }
    }
};
