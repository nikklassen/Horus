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

                    expect(res.body).to.eql({
                        vars: {},
                        funcs: {},
                        bound: {},
                        result: 2.0
                    })

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

                    // Error at equals sign
                    expect(res.body).to.eql({
                        error: 'Invalid input: at position 3'
                    })

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

                    expect(res.body).to.eql({
                        vars: { a: '1.0' },
                        funcs: {},
                        bound: {},
                        result: 1.0
                    })

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

                    expect(res.body).to.eql({
                        vars: {},
                        funcs: { a: '(x) = (1.0 + x)' },
                        bound: {},
                        result: 0.0
                    })

                    done()
                })
        })

        it('should return a bound variable', function(done) {

            request('localhost')
                .post('/api/calculate')
                .send({ input: 'a := 3' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.body).to.eql({
                        result: 3.0,
                        vars: {},
                        funcs: {},
                        bound: {
                            a: {
                                expr: '3.0',
                                value: 3.0
                            }
                        }
                    })

                    done()
                })
        })

    })

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

                    expect(res.body).to.eql({
                        vars: { a: '2.0' },
                        funcs: { a: '(x) = (2.0 + x)' },
                        bound: {
                            y: {
                                expr: 'a',
                                value: 2.0
                            }
                        }
                    })

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

            var operations = [
                { op: 'remove', path: '/vars/b' },
                { op: 'remove', path: '/funcs/b' },
                { op: 'remove', path: '/bound/z' }
            ]

            request('localhost')
                .post('/api/userInfo')
                .set('Cookie', 'user-id=testUser2')
                .set('Content-Type', 'application/json-patch+json')
                .send(JSON.stringify(operations))
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

                            expect(res.body).to.eql({
                                vars: {},
                                funcs: {},
                                bound: {},
                            })

                            done()
                        })
                })
        })
    })
})
