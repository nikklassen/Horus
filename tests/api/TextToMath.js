/* global require, describe, it */

var expect = require('expect.js')
var request = require('supertest')

describe('TextToMath Api', function() {

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

    // Test server data
    // User name: testUser
    // Vars:			a = 2.0
    // Functions: a(x) = x + 2
    //
    // User name: testUser2
    // Vars:			b = 3.0
    // Functions: b(x) = x + 3
    describe('UserInfo', function() {

        it('should return userInfo', function(done) {

            request('localhost')
                .get('/api/userInfo')
                .set('Cookie', 'user-id=testUser')
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.vars).to.eql({ a: '2.0' })
                    expect(body.funcs).to.eql({ a: '(x) = (x + 2)' })

                    done()
                })
        })

        it('should delete userInfo', function(done) {

            request('localhost')
                .del('/api/userInfo')
                .set('Cookie', 'user-id=testUser')
                .expect(204)
                .end(function(err) {
                    if (err) {
                        throw err
                    }

                    done()
                })
        })

        it('should reject content type', function(done) {

            request('localhost')
                .post('/api/userInfo')
                .set('Cookie', 'user-id=testUser2')
                .set('Content-Type', 'text/plain')
                .send('[{ "op": "remove", "path": "/var/b" }]')
                .expect(415)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.text).to.be('Content type must be application/json-patch+json')

                    done()
                })
        })

        it('should only permit remove', function(done) {

            request('localhost')
                .post('/api/userInfo')
                .set('Cookie', 'user-id=testUser2')
                .set('Content-Type', 'application/json-patch+json')
                .send('[{ "op": "add", "path": "/var/b" }]')
                .expect(400)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.text).to.be('Unpermitted operation')

                    done()
                })
        })

        it('should be unable to apply the patch', function(done) {

            request('localhost')
                .post('/api/userInfo')
                .set('Cookie', 'user-id=testUser2')
                .set('Content-Type', 'application/json-patch+json')
                .send('[{ "op": "remove", "path": "/var/nonexistantVar" }]')
                .expect(400)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.text).to.be('Unable to apply patch')

                    done()
                })
        })

        it('should delete the saved information', function(done) {

            request('localhost')
                .post('/api/userInfo')
                .set('Cookie', 'user-id=testUser2')
                .set('Content-Type', 'application/json-patch+json')
                .send('[{ "op": "remove", "path": "/var/b" }, { "op": "remove", "path": "/func/b" }]')
                .expect(200)
                .end(function(err) {
                    if (err) {
                        throw err
                    }

                    request('localhost')
                        .get('/api/userInfo')
                        .set('Cookie', 'user-id=testUser2')
                        .expect(200)
                        .end(function(err, res) {
                            if (err) {
                                throw err
                            }

                            var body = res.body

                            expect(body.vars).to.eql({})
                            expect(body.funcs).to.eql({})

                            done()
                        })
                })
        })
    })
})
