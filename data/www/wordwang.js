var wordwang = {
    debug: false,

    sendReq: function(sock, obj) {
        var payload = JSON.stringify(obj);
        console.log("wordwang: sending `" + payload + "'");
        sock.send(payload);
    },

    onResp: function(sock, f) {
        sock.onmessage = function(event) {
            console.log("wordwang: received `" + event.data + "'");
            f(JSON.parse(event.data));
        };
    }
};
