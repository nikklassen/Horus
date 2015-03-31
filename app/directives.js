(function() {
    angular.module('HorusApp')
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

    .directive('appendTo', function() {
        return {
            scope: {
                target: '=appendTo',
            },
            link: function(scope, element) {
                element.bind('dblclick', function() {
                    scope.target += element.text().trim()
                    scope.$apply();
                    document.getElementById('input').focus()
                })
            }
        }
    })
}());
