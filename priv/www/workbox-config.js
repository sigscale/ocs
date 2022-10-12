module.exports = {
	globDirectory: './',
	globIgnores: [
		"service-worker.js",
		"workbox-*.js"
	],
	globPatterns: [
		'index.html',
		'manifest.json',
		'src/sig-*.js',
      'node_modules/@webcomponents/webcomponentsjs/webcomponents-bundle.js',
		'images/*'
	],
	swDest: 'service-worker.js',
	navigateFallback: '/index.html',
	navigateFallbackAllowlist: [/^(?!.*\.js$|\/data\/).*/]
}
