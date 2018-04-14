/* jshint undef: false */
var chai = require('chai')
var chaiAsPromised = require('chai-as-promised')

chai.use(chaiAsPromised)
var expect = chai.expect
var assert = chai.assert

function extend(target) {
    var copyProp = function(source, prop) {
        target[prop] = source[prop];
    }
    for (var i = 1; i < arguments.length; i++) {
        var source = arguments[i]
        Object.keys(source).map(copyProp.bind(null, source));
    }
}

var Definition = function(type, name) {
    var id = '/' + type + '/' + name
    this._element = element(by.id(id))
    this.name = this.element(by.binding('v.name'))
    this.value = this.element(by.binding('v.value'))
}

Definition.prototype = {
    element: function(locator) {
        return this._element.element(locator)
    },
    isPresent: function() {
        return this._element.isPresent()
    }
}

var BoundDefinition = function(type, name) {
    var def = new Definition(type, name);
    extend(this, def);
    this.expr = this.element(by.binding('v.value.expr'))
    this.value = this.element(by.binding('v.value.value'))
}

extend(BoundDefinition.prototype, Definition.prototype)

var Homepage = function() {

    this.input = element(by.model('input'))
    this.result = element(by.id('result'))
    this.errorResult = element(by.id('error-result'))
    this.submitBtn = element(by.id('submit'))
    this.resetBtn = element(by.id('reset'))
    this.angleModes = element(by.tagName('buttons-radio'))
    this.clearLogsBtn = element(by.id('clear-logs-btn'))
}

Homepage.prototype = {
    get: function() {
        browser.get('http://localhost')
    },

    setInput: function(data) {
        this.input.clear()
        this.input.sendKeys(data)
    },

    getInput: function() {
        return this.input.getAttribute('value')
    },

    calculate: function() {
        this.submitBtn.click()
    },

    reset: function() {
        this.resetBtn.click()
    },

    clearLogs: function() {
        this.clearLogsBtn.click()
    },

    getDefinition: function(type, name) {
        return new Definition(type, name)
    },

    getBoundDefinition: function(type, name) {
        return new BoundDefinition(type, name)
    },

    getAngleMode: function() {
        return this.angleModes.element(by.css('.active')).getText()
    },

    setAngleMode: function(mode) {
        this.angleModes.element(by.partialButtonText(mode)).click()
    }
}

// Test server data
// User name: testUser
// Vars:      a = 2.0
// Functions: a(x) = x + 2
// Bound:     y := a
//
// User name: testUser2
// Vars:      b = 3.0
// Functions: b(x) = x + 3
// Bound:     z := b
describe('homepage', function () {

    before(function() {
        browser.driver.manage().window().setSize(1280, 1024);
        browser.get('/')
        browser.manage().addCookie('user-id', 'testUser')
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

        expect(homepage.errorResult.getText()).to.eventually.equal('Invalid input: at position 3\n1 = ...')
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
        expect(f.value.getText()).to.eventually.equal('x + 2')
    })

    it('should have existing bound variables', function() {
        var homepage = new Homepage()
        homepage.get()

        var v = homepage.getBoundDefinition('vars', 'y')
        expect(v.isPresent()).to.eventually.equal(true)

        expect(v.name.getText()).to.eventually.equal('y')
        expect(v.value.getText()).to.eventually.equal('2.0')
        expect(v.expr.getText()).to.eventually.equal('a')
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
        expect(f.value.getText()).to.eventually.equal('3 * r')
    })

    it('should append a log to the input', function() {
        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('2 * 3')
        homepage.calculate();

        var column = element(by.repeater('log in logs').row(0).column('result'))
        assert.eventually.equal(column.isPresent(), true)

        homepage.setInput('six = ')
        browser.actions().doubleClick(column).perform()

        expect(homepage.getInput()).to.eventually.equal('six = 6.0')
    })

    it('should append a function to the input', function() {
        var homepage = new Homepage()
        homepage.get()

        var f = homepage.getDefinition('funcs', 'a')
        assert.eventually.equal(f.isPresent(), true)

        homepage.setInput('1 + ')
        browser.actions().doubleClick(f.value).perform()

        expect(homepage.getInput()).to.eventually.equal('1 + x + 2')
    })

    it('should delete a bound variable', function() {
        var homepage = new Homepage()
        homepage.get()

        var v = homepage.getBoundDefinition('vars', 'y')
        assert.eventually.equal(v.isPresent(), true);

        // Click the 'X' button
        v.element(by.css('.glyphicon-remove')).click()
        assert.eventually.equal(v.isPresent(), false)
    })

    it('should reset the user\'s environment', function() {

        browser.manage().addCookie('user-id', 'testUser2')

        var homepage = new Homepage()
        homepage.get()

        assert.eventually.notEqual(element.all(by.repeater('v in env')).count(), 0)

        homepage.reset()

        expect(element.all(by.repeater('v in env')).count()).to.eventually.equal(0)
    })

    it('should have the correct angle mode', function() {

        var homepage = new Homepage()
        homepage.get()

        // Starts off in radians
        expect(homepage.getAngleMode()).to.eventually.equal('Rad')

        // Calculation saves the user's preference
        homepage.setAngleMode('Deg')
        homepage.setInput('1 + 1')
        homepage.calculate()

        // Refresh page to ensure preference persists
        homepage.get()
        expect(homepage.getAngleMode()).to.eventually.equal('Deg')
    })

    it('should show the help dialog', function() {

        var homepage = new Homepage()
        homepage.get()

        element(by.id('help-btn')).click()

        assert.eventually.equal(element(by.css('.modal-dialog')).isPresent(), true)
    })

    it('should add the calculation to the log', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('2 * 3')
        homepage.calculate();

        assert.eventually.equal(element(by.repeater('log in logs').row(0).column('input')).getText(), '2 * 3')
    })

    it('should only keep 10 logs', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('1 + 1')
        for (var i = 0; i <= 11; i++) {
            homepage.calculate();
        }

        assert.eventually.equal(element.all(by.repeater('log in logs')).count(), 10)
    })

    it('should clear the logs', function() {

        var homepage = new Homepage()
        homepage.get()

        homepage.setInput('1 + 1')
        homepage.calculate()

        expect(element.all(by.repeater('log in logs')).count()).to.eventually.be.above(0)

        homepage.clearLogs()

        assert.eventually.equal(element.all(by.repeater('log in logs')).count(), 0)
    })
})
