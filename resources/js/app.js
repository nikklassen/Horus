(function () {
    'use strict';

    var app = angular.module('HorusApp', ['ngSanitize', 'angularModalService', 'angularLocalStorage'])

    app.controller('ModalController', ['$scope', 'close', function ($scope, close) {
        $scope.close = close
    }])

    app.controller('HorusCtrl', ['$scope', '$http', 'ModalService', 'storage', function ($scope, $http, ModalService, storage) {

        $scope.env = {}

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
        storage.bind($scope, 'logs', { defaultValue: [] })

        $scope.setInput = function(text) {
            $scope.input = text
            document.getElementById('input').focus()
        }

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

    .filter('num', function() {
        var THRESHOLD = 8;
        var isNumber = /^-?\d*(\.\d*)?$/

        function truncate(s, digits) {
            var dIdx = s.indexOf('.')
            if (dIdx === -1) {
                return s
            }

            // Decimal place is always at 1 now, plus 1 for the decimal point itself
            var numLength;
            if (digits === undefined) {
                numLength = s.length
            } else {
                numLength = Math.min(s.length, dIdx + digits + 1)
            }
            return s.slice(0, numLength)
        }

        // Default is show all digits
        return function(v, digits) {
            if (!isNumber.test(v) || v.length === 0) {
                return v;
            }

            var s = '';
            if (v[0] === '-') {
                s += '-'
                v = v.slice(1)
            }

            // 0 or 0.000
            if (!v.match(/[1-9]/)) {
                return v;
            }

            var firstNum = v.match(/[1-9]/).index
            var dIdx = v.indexOf('.')
            if (dIdx === -1) {
                dIdx = v.length
            }

            // 1 > v > 0
            if (dIdx - firstNum < 0) {
                s += v[firstNum] + '.' + v.slice(firstNum + 1)
                s = truncate(s, digits);
                s += 'e' + (dIdx - firstNum);
            }
            // v > 1 * 10^THRESHOLD
            else if (dIdx > THRESHOLD) {
                s += v[0] + '.' + v.slice(1, dIdx) + v.slice(dIdx + 1)
                s = truncate(s, digits);
                s += 'e' + (dIdx - 1)
            } else {
                s += v
                var n = dIdx - 3
                while (n > 0) {
                    s = s.slice(0, n) + ',' + s.slice(n)
                    n -= 3
                }
                s = truncate(s, digits);
            }

            return s
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

    .directive('number', function() {
        return {
            restrict: 'E',
            scope: { value: '=' },
            controller: function($scope, $filter) {
                if (!_.isString($scope.value)) {
                    $scope.value = $scope.value.toString()
                } else if ($scope.value === '') {
                    $scope.value = '0'
                }

                function updateDisplayValue() {
                    $scope.dval = $filter('num')($scope.value, $scope.digits)
                    $scope.hasMoreDigits = $scope.dval !== $filter('num')($scope.value)
                }

                $scope.$watch('value', function() {
                    $scope.digits = 3
                    updateDisplayValue()
                })

                $scope.addDigits = function() {
                    $scope.digits += 10
                    updateDisplayValue()
                }

                $scope.digits = 3
            },
            template:
                '<span class="result-formatted">{{dval}}</span>' +
                '<button class="btn btn-default" ng-show="hasMoreDigits" ng-click="addDigits()">More digits</button>'
        }
    })
}())
