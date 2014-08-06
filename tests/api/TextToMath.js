/* global require, describe, it */

var expect = require('expect.js')
var request = require('supertest')

describe('TextToMath Api', function() {

    describe('Calculate', function() {

        it('should return numeric result', function(done) {

            request('localhost')
                .post('/api/calculate')
                .send({ input: '1+1' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('2.0')
                    expect(body.vars).to.eql({})
                    expect(body.funcs).to.eql({})

                    done()
                })
        })

        it('should return an error', function(done) {

            request('localhost')
                .post('/api/calculate')
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
                .send({ input: 'a = 1'})
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('1.0')
                    expect(body.vars).to.eql({ a: '1.0' })
                    expect(body.funcs).to.eql({})

                    done()
                })
        })

        it('should return a function', function(done) {

            request('localhost')
                .post('/api/calculate')
                .send({ input: 'a(x) = x + 1' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    var body = res.body
                    expect(body.result).to.be('0.0')
                    expect(body.vars).to.eql({})
                    expect(body.funcs).to.eql({ a: '(x) = (1.0 + x)' })

                    done()
                })
        })

    })

    // Test server data
    // User name: testUser
    // Vars:			a = 2.0
    // Functions: a(x) = 2.0 + x
    //
    // User name: testUser2
    // Vars:			b = 3.0
    // Functions: b(x) = 3.0 + x
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
                    expect(body.funcs).to.eql({ a: '(x) = (2.0 + x)' })

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
                .send('[{ "op": "remove", "path": "/vars/b" }, { "op": "remove", "path": "/funcs/b" }]')
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
