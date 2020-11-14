module.exports = {
	staticFileGlobs: [
		'/index.html',
		'/manifest.json',
		'/node_modules/@webcomponents/webcomponentsjs/webcomponents-bundle.js',
		'/src/*.js',
		'/images/*'
	],
	navigateFallback: '/index.html',
	navigateFallbackWhitelist: [/^(?!.*\.js$|\/data\/).*/]
}
