(function () {
    'use strict';

    angular.module('TextToMathApp', ['ngSanitize'])

    .controller('TextToMathCtrl', function ($scope, $http) {

        $scope.env = {}
        $scope.resultClass = ''

        function formatForDisplay(obj, type) {
            var displayName = obj.name,
                displayValue = obj.value

            switch (type) {
                case 'funcs':
                    var sides = (obj.name + obj.value).split('=')
                    displayName = sides[0].trim()
                    displayValue = sides[1].trim()
                    break;
                case 'bound':
                    displayValue = obj.value.value + ' = ' + obj.value.expr
            }
            obj.name = displayName
            obj.value = displayValue
        }

        var addToEnv = function(data) {
            _.forOwn(data, function(contents, type) {
                _.forOwn(contents, function(value, name) {
                    var obj = {
                        id: '/' + type + '/' + name,
                        name: name,
                        value: value
                    }
                    formatForDisplay(obj, type)
                    $scope.env[obj.id] = obj
                })
            })
        }

        $scope.result = ''
        $scope.input = ''
        $scope.submit = function() {
            $http.post(
                '/api/calculate',
                { input: $scope.input },
                { headers: { 'Content-Type': 'application/json' } }
            ).success(function(data) {
                addToEnv(data)
                $scope.result = data.result
                $scope.resultClass = ''
            }).error(function(data) {
                $scope.result = data.error
                var errPos = data.error.match(/position (\d+)/)
                if (errPos !== null) {
                    var badStr = $scope.input.slice(0, errPos[1])
                    var len = badStr.length
                    $scope.result += '<br />';
                    if (len > 10) {
                        $scope.result += '... ' + badStr.slice(len-10, len)
                    } else {
                        $scope.result += badStr
                    }
                }

                $scope.resultClass = 'error'
            })
        }

        $scope.remove = function(val) {
            $http.post(
                '/api/userInfo',
                [{ 'op': 'remove', 'path': val.id }],
                { headers: { 'Content-Type': 'application/json-patch+json' } }
            ).success(function() {
                delete $scope.env[val.id]
            })
        }

        $scope.reset = function() {
            $http.delete(
                '/api/userInfo'
            ).success(function() {
                $scope.env = {}
            })
        }

        // Load initial view
        $http.get('/api/userInfo')
        .success(addToEnv)
    })

    .filter('objectToArray', function() {
        return function (obj) {
            return _.toArray(obj)
        }
    })
}())
