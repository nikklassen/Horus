module.exports = function(config) {
  config.set({
    basePath: '../',

    frameworks: ['mocha', 'chai'],

    files: [
        'tests/test_libs/angular.min.js',
        'tests/test_libs/*.js',
        'node_modules/angular-mocks/angular-mocks.js',
        'resources/js/**/*.js',
        'tests/unit/*.js'
    ],

    reporters: ['spec'],
    port: 9876,
    colors: true,
    autoWatch: false,
    browsers: ['Chrome'],
    singleRun: true
  });
};
