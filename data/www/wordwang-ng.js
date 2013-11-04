var wwApp = angular.module('wordwang', []);
// var wwApp = angular.module('wordwang', []).
//     factory('wwState', function($window) {
//         return new WWState(ww.host);
//     });
 
wwApp.controller('WWCtrl', function WWCtrl($scope) {
    var wwState = new WWState(ww.host);

    // Just a dummy story to start with:
    $scope.story = {
        id: null,
        users: {},
        blocks: [],
        candidates: {}
    };

    $scope.create = function() {
        wwState.create();
    };

    $scope.candidate = function() {
        wwState.candidate($scope.blockInput);
    };

    $scope.join = function() {
        wwState.join();
    };

    $scope.vote = function(user) {
        wwState.vote(user);
    };

    $scope.getStory = function() {
        wwState.getStory();
    };

    // Keep the two the same
    wwState.onResp(null, function(_) {
        $scope.story = wwState.story;
        $scope.$digest();
    });

    // Join on room creation
    wwState.onResp('created', function(_) {
        createDiv.style.display = 'none';
        storyDiv.style.display = 'block';
        wwState.getStory();
        wwState.join();
        window.location = ww.storyUrl(wwState.storyId);
    });

    // TODO there's probably a way to get this...
    var createDiv = document.getElementById('create');
    var storyDiv = document.getElementById('story');
    var storyId = ww.getHash();
    if (ww.getHash() === null) {
        createDiv.style.display = 'block';
    } else {
        storyDiv.style.display = 'block';
        storyId = ww.getHash();
        wwState.storyId = storyId;
    }
});
