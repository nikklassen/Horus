'use strict'

var textToMathApp = angular.module('TextToMathApp', ['ngSanitize'])

textToMathApp.controller('TextToMathCtrl', function ($scope, $http) {

    $scope.env = {
        vars: {},
        funcs: {}
    }

    $scope.resultClass = ''

    var addToEnv = function(data) {
        for (var key in data.vars) {
            $scope.env.vars[key] = {
                name: key,
                value: data.vars[key],
                type: 'vars'
            }
        }
        for (key in data.funcs) {
            $scope.env.funcs[key] = {
                name: key,
                value: data.funcs[key],
                type: 'funcs'
            }
        }
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
                if (len > 10) {
                    $scope.result += '<br />...' + badStr.slice(len-10, len)
                } else {
                    $scope.result += badStr
                }
            }

            $scope.resultClass = 'error'
        })
    }

    $scope.remove = function(val) {
        var path = '/' + val.type + '/' + val.name

        $http.post(
            '/api/userInfo',
            [{ 'op': 'remove', 'path': path }],
            { headers: { 'Content-Type': 'application/json-patch+json' } }
        ).success(function() {
            delete $scope.env[val.type][val.name]
        })
    }

    $scope.reset = function() {
        $http.delete(
            '/api/userInfo'
        ).success(function() {
            $scope.env.vars = {}
            $scope.env.funcs = {}
        })
    }

    $scope.getFuncName = function(val) {
        var name = val.name + val.value
        return name.slice(0, name.indexOf('=') - 1)
    }

    $scope.getFuncValue = function(val) {
        var value = val.name + val.value
        return value.slice(value.indexOf('=') + 2)
    }

    // Load initial view
    $http.get('/api/userInfo')
         .success(addToEnv)
})
