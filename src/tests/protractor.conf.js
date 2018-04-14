exports.config = {
    specs: [
        'e2e/*.js'
    ],

    directConnect: true,

    capabilities: {
        'browserName': 'chrome'
    },

    baseUrl: 'http://localhost/',

    framework: 'mocha',

    mochaOpts: {
        reporter: 'spec',
        timeout: 11000,
        slow: 2000
    }
}
