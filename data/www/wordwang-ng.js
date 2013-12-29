var wwApp = angular.module('wordwang', []);

wwApp.controller('WWCtrl', function WWCtrl($scope, $http) {
    // Just a dummy story to start with:
    $scope.story = {
        id: null,
        users: [],
        blocks: [],
        candidates: {}
    };

    var wwState;
    wwState = new WWState(ww.host, function(_) {
        // TODO there's probably a way to get this...
        var createDiv = document.getElementById('create');
        var storyDiv = document.getElementById('story');
        var actions = document.getElementById('actions');
        var joinButton = document.getElementById('joinButton');

        $scope.create = function() {
            $http.get('/create').
                success(function(data, _status, _headers, _config) {
                    wwState.roomId = JSON.parse(data);
                    createDiv.style.display = 'none';
                    storyDiv.style.display = 'block';
                    wwState.getStory();
                    window.location = ww.roomUrl(wwState.roomId);
                }).
                error(function(_data, _status, _headers, _config) {
                    ww.errorLog("error while creating story");
                });
        };

        $scope.candidate = function() {
            wwState.candidate($scope.blockInput);
        };

        $scope.join = function() {
            wwState.join();
        };

        $scope.vote = function(candidateId) {
            wwState.vote(candidateId);
        };

        $scope.getStory = function() {
            wwState.getStory();
        };

        $scope.closeVoting = function() {
            wwState.closeVoting();
        };

        // Keep the two the same
        wwState.onResp(null, function(_) {
            $scope.story = wwState.story;
            $scope.$digest();
        });

        // Show candidate on join
        wwState.onResp('join', function(_) {
            actions.style.display = 'block';
            joinButton.style.display = 'none';
        });

        var roomId = ww.getHash();
        if (roomId === null) {
            createDiv.style.display = 'block';
        } else {
            storyDiv.style.display = 'block';
            roomId = ww.getHash();
            wwState.roomId = roomId;
            wwState.getStory();
        }
    });
});
