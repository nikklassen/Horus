exports.config = {
    specs: [
        'e2e/*.js'
    ],

    capabilities: {
        'browserName': 'chrome'
    },

    chromeOnly: true,

    baseUrl: 'http://localhost/',

    framework: 'mocha',

    mochaOpts: {
        reporter: 'spec',
        timeout: 11000,
        slow: 2000
    }
}
