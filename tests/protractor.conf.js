exports.config = {
    allScriptsTimeout: 11000,

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
        reporter: 'spec'
    }
}
