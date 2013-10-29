// Globals
var host   = "ws://localhost:8000/ws";
wordwang.debug = true;

// Tests
var sock1 = new WebSocket(host);
var sock2 = new WebSocket(host);
var sock3 = new WebSocket(host);

sock1.onopen = function(_) {
sock2.onopen = function(_) {
sock3.onopen = function(_) {
    var req1 = {"body": {"tag": "create"}};
    sendAndRecv(sock1, req1, function(resp1) {
        checkTag(resp1, "created");
        var story = resp1.story;
        var req2 = {"story": story, "body": {"tag": "join"}};
        sendAndRecv(sock1, req2, function(resp2) {
            checkTag(resp2, "joined");
        });
    });
}}};

// Utils
function sendAndRecv(sock, req, f) {
    wordwang.onResp(sock, f);
    wordwang.sendReq(sock, req);
}

function checkTag(resp, tag) {
    if (resp.tag !== tag) {
        throw ("Expecting tag `" + tag + "', got `" + resp.tag + "'");
    }
}
