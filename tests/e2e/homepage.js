/* jshint undef: false */
var chai = require('chai')
var chaiAsPromised = require('chai-as-promised')

chai.use(chaiAsPromised)
var expect = chai.expect
var assert = chai.assert

var Definition = function(type, name) {
    var id = '/' + type + '/' + name
    this._element = element(by.id(id))
    this.name = this._element.element(by.binding('v.name'))
    this.value = this._element.element(by.binding('v.value'))
}

Definition.prototype = {
    element: function(locator) {
        return this._element.element(locator)
    },
    isPresent: function() {
        return this._element.isPresent()
    }
}

var Homepage = function() {

    this.input = element(by.model('input'))
    this.result = element(by.binding('result'))
    this.submitBtn = element(by.id('submit'))
    this.resetBtn = element(by.id('reset'))

}

Homepage.prototype = {
    get: function() {
        browser.get('http://localhost')
    },

    setInput: function(data) {
        this.input.sendKeys(data)
    },

    calculate: function() {
        this.submitBtn.click()
    },

    reset: function() {
        this.resetBtn.click()
    },

    getDefinition: function(type, name) {
        return new Definition(type, name)
    }
}

// Test server data
// User name: testUser
// Vars:      a = 2.0
// Functions: a(x) = 2.0 + x
// Bound:     y := a
//
// User name: testUser2
// Vars:      b = 3.0
// Functions: b(x) = 3.0 + x
// Bound:     z := b
describe('homepage', function () {

    beforeEach(function() {
        var ptor = protractor.getInstance()
        browser.get('/')
        ptor.manage().addCookie('user-id', 'testUser')
    }, 10000)

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

        expect(homepage.result.getText()).to.eventually.equal('Invalid input: at position 3\n1 =')
        expect(homepage.result.getAttribute('class')).to.eventually.contain('error')
    })

    it('should have existing variables', function() {
        var homepage = new Homepage()
        homepage.get()

        var v = homepage.getDefinition('vars', 'a')
        assert.eventually.equal(v.isPresent(), true)

        expect(v.name.getText()).to.eventually.equal('a')
        expect(v.value.getText()).to.eventually.equal('2.0')
    })

    it('should have existing functions', function() {
        var homepage = new Homepage()
        homepage.get()

        var f = homepage.getDefinition('funcs', 'a')
        assert.eventually.equal(f.isPresent(), true)

        expect(f.name.getText()).to.eventually.equal('a(x)')
        expect(f.value.getText()).to.eventually.equal('(2.0 + x)')
    })

    it('should have existing bound variables', function() {
        var homepage = new Homepage()
        homepage.get()

        var b = homepage.getDefinition('bound', 'y')
        assert.eventually.equal(b.isPresent(), true)

        expect(b.name.getText()).to.eventually.equal('y')
        expect(b.value.getText()).to.eventually.equal('2.0 = a')
    })

    it('should replace existing variable', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('a = 4')
        homepage.calculate()

        var v = homepage.getDefinition('vars', 'a')
        expect(v.value.getText()).to.eventually.equal('4.0')
    })

    it('should add a function', function() {
        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('q(r) = 3 * r')
        homepage.calculate()

        var f = homepage.getDefinition('funcs', 'q')
        assert.eventually.equal(f.isPresent(), true)

        expect(f.name.getText()).to.eventually.equal('q(r)')
        expect(f.value.getText()).to.eventually.equal('(3.0 * r)')
    })

    it('should delete a bound variable', function() {
        var homepage = new Homepage()
        homepage.get()

        var b = homepage.getDefinition('bound', 'y')

        // Click the 'X' button
        b.element(by.css('span')).click()
        assert.eventually.equal(b.isPresent(), false)
    })

    it('should reset the user\'s environment', function() {

        protractor.getInstance().manage().addCookie('user-id', 'testUser2')

        var homepage = new Homepage()
        homepage.get()

        assert.eventually.notEqual(element.all(by.repeater('v in env')).count(), 0)

        homepage.reset()

        expect(element.all(by.repeater('v in env')).count()).to.eventually.equal(0)
    })
})
