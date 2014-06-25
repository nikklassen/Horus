var expect = require('expect.js')
var request = require('supertest')

describe('TextToMath Api', function() {

    var userId = ""

    describe('Calculate', function() {

        it('should return numeric result', function(done) {

            request('localhost')
                .post('/api/calculate')
                .type('form')
                .send({ input: '1+1' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('2.0')
                    expect(body.newVars).to.eql({})
                    expect(body.newFuncs).to.eql({})

                    done()
                })
        })

        it('should return an error', function(done) {

            request('localhost')
                .post('/api/calculate')
                .type('form')
                .send({ input: '1 = 1'})
                .expect(400)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.body.error).to.be('Invalid input')

                    done()
                })
        })

        it('should return a variable', function(done) {

            request('localhost')
                .post('/api/calculate')
                .type('form')
                .send({ input: 'a = 1'})
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('1.0')
                    expect(body.newVars).to.eql({ a: '1.0' })
                    expect(body.newFuncs).to.eql({})

                    done()
                })
        })

        it('should return a function', function(done) {

            request('localhost')
                .post('/api/calculate')
                .type('form')
                .send({ input: 'a(x) = x + 1' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('0.0')
                    expect(body.newVars).to.eql({})
                    expect(body.newFuncs).to.eql({ a: '(x) = (x + 1)' })

                    done()
                })
        })

    })

    // TODO test userInfo routes once the test server has been written
})
