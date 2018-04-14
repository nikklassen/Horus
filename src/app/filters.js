(function() {
    angular.module('HorusApp')
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
                var trailingDigits = firstNum === v.length - 1 ? '' : '.' + v.slice(firstNum + 1);

                s += v[firstNum] + trailingDigits
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
}());
