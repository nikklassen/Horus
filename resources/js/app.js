(function () {
    'use strict';

    var app = angular.module('TextToMathApp', ['ngSanitize', 'angularModalService'])

    app.controller('ModalController', ['$scope', 'close', function ($scope, close) {
        $scope.close = close
    }])

    app.controller('TextToMathCtrl', ['$scope', '$http', 'ModalService', function ($scope, $http, ModalService) {

        $scope.env = {}
        $scope.resultClass = ''

        function formatForDisplay(obj, type) {
            var displayName, displayValue

            switch (type) {
                case 'funcs':
                    displayName = obj.value.decl
                    displayValue = obj.value.def
                    break
                case 'vars':
                    displayName = obj.name
                    displayValue = obj.value.value
                    if (obj.value.expr !== undefined) {
                         displayValue += ' = ' + obj.value.expr
                    }
                    break
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
                'api/calculate',
                {
                    input: $scope.input,
                    prefs: {
                        isRadians: $scope.angleMode === 'Rad'
                    }
                },
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
                    $scope.result += '<br />'
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
                'api/userInfo',
                [{ 'op': 'remove', 'path': val.id }],
                { headers: { 'Content-Type': 'application/json-patch+json' } }
            ).success(function() {
                delete $scope.env[val.id]
            })
        }

        $scope.reset = function() {
            $http.delete(
                'api/userInfo'
            ).success(function() {
                $scope.env = {}
            })
        }

        // Degree mode radio buttons
        $scope.angleModes = ['Deg', 'Rad']

        $scope.showModal = function() {
            ModalService.showModal({
                templateUrl: 'help.html',
                controller: 'ModalController'
            })
        }

        // Load initial view
        $http.get('api/userInfo')
        .success(function(data) {
            $scope.angleMode = data.prefs.isRadians ? 'Rad' : 'Deg'
            addToEnv({
                funcs: data.funcs,
                vars: data.vars
            })
        })
    }])

    .filter('objectToArray', function() {
        return function (obj) {
            return _.toArray(obj)
        }
    })

    .directive('buttonsRadio', function() {
        return {
            restrict: 'E',
            scope: { model: '=', options:'='},
            controller: function($scope){
                $scope.activate = function(option){
                    $scope.model = option
                };      
            },
            template: '<button type="button" class="btn btn-default" '+
                        'ng-class="{active: option == model}" '+
                        'ng-repeat="option in options" '+
                        'ng-click="activate(option)">{{option}}'+
                      '</button>'
        }
    })
}())
