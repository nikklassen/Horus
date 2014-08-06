/* jshint undef: false */
var chai = require('chai')
var chaiAsPromised = require('chai-as-promised')

chai.use(chaiAsPromised)
var expect = chai.expect
var assert = chai.assert

var Homepage = function() {

    this.input = element(by.model('input'))
    this.result = element(by.binding('result'))
    this.submitBtn = element(by.id('submit'))
    this.resetBtn = element(by.id('reset'))

    this.get = function() {
        browser.get('http://localhost')
    }

    this.setInput = function(data) {
        this.input.sendKeys(data)
    }

    this.calculate = function() {
        this.submitBtn.click()
    }

    this.reset = function() {
        this.resetBtn.click()
    }
}

// Test server data
// User name: testUser
// Vars:			a = 2.0
// Functions: a(x) = 2.0 + x
//
// User name: testUser2
// Vars:			b = 3.0
// Functions: b(x) = 3.0 + x
describe('homepage', function () {

    beforeEach(function() {
        var ptor = protractor.getInstance()
        browser.get('/')
        ptor.manage().addCookie('user-id', 'testUser')
    })

    it('should calculate 1 + 1', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('1 + 1')

        homepage.calculate()

        expect(homepage.result.getText()).to.eventually.equal('2.0')
    })

    it('should be invalid input', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('1 = 1')

        homepage.calculate()

        expect(homepage.result.getText()).to.eventually.equal('Invalid expression')
        expect(homepage.result.getAttribute('class')).to.eventually.contain('error')
    })

    it('should have existing variables', function() {
        var homepage = new Homepage()
        homepage.get()

        var rowBy = by.repeater('v in env.vars').row(0)

        assert.eventually.equal(element.all(by.repeater('v in env.vars')).count(), 1)

        expect(element(rowBy.column('v.name')).getText()).to.eventually.equal('a')
        expect(element(rowBy.column('v.value')).getText()).to.eventually.equal('2.0')
    })

    it('should have existing functions', function() {
        var homepage = new Homepage()
        homepage.get()

        var rowBy = by.repeater('v in env.funcs').row(0)

        assert.eventually.equal(element.all(by.repeater('v in env.funcs')).count(), 1)

        expect(element(rowBy.column('getFuncName(v)')).getText()).to.eventually.equal('a(x)')
        expect(element(rowBy.column('getFuncValue(v)')).getText()).to.eventually.equal('(2.0 + x)')
    })

    it('should replace existing variable', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('a = 4')
        homepage.calculate()

        var rowBy = by.repeater('v in env.vars').row(0)

        expect(element(rowBy.column('v.name')).getText()).to.eventually.equal('a')
        expect(element(rowBy.column('v.value')).getText()).to.eventually.equal('4.0')
        expect(element.all(by.repeater('v in env.vars')).count()).to.eventually.equal(1)
    })

    it('should replace existing function', function() {
        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('a(y) = y + 3')
        homepage.calculate()

        var rowBy = by.repeater('v in env.funcs').row(0)

        expect(element(rowBy.column('getFuncName(v)')).getText()).to.eventually.equal('a(y)')
        expect(element(rowBy.column('getFuncValue(v)')).getText()).to.eventually.equal('(3.0 + y)')
        expect(element.all(by.repeater('v in env.funcs')).count()).to.eventually.equal(1)
    })

    it('should add a variable', function() {
        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('z = 2')

        homepage.calculate()

        var varBy = by.repeater('v in env.vars')

        assert.eventually.equal(element.all(varBy).count(), 2)
        expect(element(varBy.row(1).column('v.name')).getText()).to.eventually.equal('z')
        expect(element(varBy.row(1).column('v.value')).getText()).to.eventually.equal('2.0')
        expect(homepage.result.getText()).to.eventually.equal('2.0')
    })

    it('should delete a variable', function() {
        var homepage = new Homepage()
        homepage.get()

        var varBy = by.repeater('v in env.vars')

        assert.eventually.equal(element.all(varBy).count(), 2)

        // Click the 'X' button
        element(varBy.row(0)).element(by.css('span')).click()

        expect(element.all(varBy).count()).to.eventually.equal(1)
    })

    it('should add a function', function() {
        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('z(x) = x * 2')

        homepage.calculate()

        var funcBy = by.repeater('v in env.funcs')

        assert.eventually.equal(element.all(funcBy).count(), 2)
        expect(element(funcBy.row(1).column('getFuncName(v)')).getText()).to.eventually.equal('z(x)')
        expect(element(funcBy.row(1).column('getFuncValue(v)')).getText()).to.eventually.equal('(2.0 * x)')
        expect(homepage.result.getText()).to.eventually.equal('0.0')
    })

    it('should delete the function', function() {
        var homepage = new Homepage()
        homepage.get()

        var funcBy = by.repeater('v in env.funcs')

        assert.eventually.equal(element.all(funcBy).count(), 2)

        // Click the 'X' button
        element(funcBy.row(0)).element(by.css('span')).click()

        expect(element.all(funcBy.row(0)).count()).to.eventually.equal(1)
    })

    it('should reset the user\'s environment', function() {

        protractor.getInstance().manage().addCookie('user-id', 'testUser2')

        var homepage = new Homepage()
        homepage.get()

        assert.eventually.equal(element.all(by.repeater('v in env.vars')).count(), 1)
        assert.eventually.equal(element.all(by.repeater('v in env.funcs')).count(), 1)

        homepage.reset()

        expect(element.all(by.repeater('v in env.vars')).count()).to.eventually.equal(0)
        expect(element.all(by.repeater('v in env.funcs')).count()).to.eventually.equal(0)
    })
})
