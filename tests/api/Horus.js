/* global require, describe, it */

var expect = require('expect.js')
var request = require('supertest')

// Test server data
// Preferences
// isRadians: true
//
// User name: testUser
// Vars:      a = 2.0,
//            y := a
// Functions: a(x) = x + 2
//
// User name: testUser2
// Vars:      b = 3.0
//            z := b
// Functions: b(x) = x + 3
describe('Horus Api', function() {

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
                        result: 2
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
                        vars: {
                            a: {
                                value: 1
                            }
                        },
                        funcs: {},
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
                        funcs: {
                            a: {
                                decl: 'a(x)',
                                def: 'x + 1'
                            }
                        },
                        result: 0.0
                    })

                    done()
                })
        })

        it('should return a bound variable', function(done) {

            request('localhost')
                .post('/api/calculate')
                .set('Cookie', 'user-id=testUser2')
                .send({ input: 'y := 4 * b' })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.body).to.eql({
                        result: 12,
                        vars: {
                            b: {
                                value: 3
                            },
                            y: {
                                expr: '(4.0 * b)',
                                value: 12
                            },
                            z: {
                                expr: 'b',
                                value: 3
                            }
                        },
                        funcs: {}
                    })

                    done()
                })
        })

        it('should overwrite the current user prefs', function(done) {
            request('localhost')
                .post('/api/calculate')
                .set('Cookie', 'user-id=testUser2')
                .send({
                    input: 'sin(90)',
                    prefs: {
                        isRadians: false
                    }
                 })
                .expect(200)
                .end(function(err, res) {
                    if (err) {
                        throw err
                    }

                    expect(res.body.result).to.eql(1)

                    done()
                })
        })

    })

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
                        vars: {
                            a: {
                                value: 2
                            },
                            y: {
                                expr: 'a',
                                value: 2
                            }
                        },
                        funcs: {
                            a: {
                                decl: 'a(x)',
                                def: 'x + 2'
                            }
                        },
                        prefs: {
                            isRadians: true
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
                { op: 'remove', path: '/vars/y' },
                { op: 'remove', path: '/vars/z' },
                { op: 'remove', path: '/funcs/b' },
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

                            expect(res.body.vars).to.eql({
                                b: {
                                    value: 3
                                }
                            })
                            expect(res.body.funcs).to.eql({})

                            done()
                        })
                })
        })
    })
})
