(function () {
    'use strict';

    var app = angular.module('HorusApp', ['ngSanitize', 'angularModalService', 'angularLocalStorage'])

    app.controller('ModalController', ['$scope', 'close', function ($scope, close) {
        $scope.close = close
    }])

    app.controller('HorusCtrl', ['$scope', '$http', 'ModalService', 'storage', horusCtrl]);
    
    function horusCtrl($scope, $http, ModalService, storage) {

        $scope.env = {
            constExpr: {},
            dynExpr: {}
        }

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

                    if (obj.value.expr !== undefined) {
                        $scope.env.dynExpr[obj.id] = obj
                    } else {
                        formatForDisplay(obj, type)
                        $scope.env.constExpr[obj.id] = obj
                    }
                })
            })
        }

        $scope.result = ''
        $scope.input = ''
        storage.bind($scope, 'logs', { defaultValue: [] })

        $scope.clearLogs = function() {
            $scope.logs = []
        }

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
                $scope.hasError = false

                // Good limit for now, could be configurable later
                if ($scope.logs.length === 10) {
                    $scope.logs.pop()
                }

                $scope.logs.unshift({
                    input: $scope.input,
                    result: data.result
                })
            }).error(function(data) {
                $scope.error = data.error
                $scope.hasError = true

                var errPos = data.error.match(/position (\d+)/)
                if (errPos !== null) {
                    var badStr = $scope.input.slice(0, errPos[1])
                    var len = badStr.length
                    $scope.error += '<br />'
                    if (len > 10) {
                        $scope.error += '... ' + badStr.slice(len-10, len)
                    } else {
                        $scope.error += badStr
                    }

                    $scope.error += ' ...'
                }
            })
        }

        $scope.remove = function(val) {
            $http.post(
                'api/userInfo',
                [{ 'op': 'remove', 'path': val.id }],
                { headers: { 'Content-Type': 'application/json-patch+json' } }
            ).success(function() {
                delete $scope.env.dynExpr[val.id]
                delete $scope.env.constExpr[val.id]
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

        $scope.isEmptyObject = function(obj) {
            return _.toArray(obj).length === 0
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
    }
}())
