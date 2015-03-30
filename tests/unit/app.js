/* jshint mocha: true */
/* global expect, module, inject */

describe('HorusApp', function() {

    var $num

    beforeEach(module('HorusApp'))
    beforeEach(inject(function($filter){
        $num = $filter('num')
    }))

    describe('Num filter', function() {

        it('should not change 0.0', function() {
            expect($num('0.0')).to.equal('0.0')
        })

        it('should add commas for numbers below threshold with decimal', function() {
            expect($num('10000000.')).to.equal('10,000,000.')
        })

        it('should add commas for numbers below threshold without decimal', function() {
            expect($num('10000000')).to.equal('10,000,000')
        })

        it('should not trim decimal places', function() {
            expect($num('1000.1234567891')).to.equal('1,000.1234567891')
        })

        it('should do positive exponent', function() {
            expect($num('123456789')).to.equal('1.23456789e8')
        })

        it('should truncate positive exponent', function() {
            expect($num('123456789', 4)).to.equal('1.2345e8')
        })

        it('should do negative exponent', function() {
            expect($num('0.0000012345', 4)).to.equal('1.2345e-6')
        })

        it('should truncate negative exponent', function() {
            expect($num('0.00000123456789', 4)).to.equal('1.2345e-6')
        })

        it('should not add trailing zero', function() {
            expect($num('0.0001')).to.equal('1e-4')
        })
    })
})
