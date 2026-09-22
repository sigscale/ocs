import { defineConfig } from 'vite';
import { VitePWA } from 'vite-plugin-pwa';

export default defineConfig({
	root: "./",
	resolve: {
		preserveSymlinks: true 
	},
	build: {
		outDir: "dist",
		emptyOutDir: true,
		target: 'es2020',
	},
	plugins: [
		VitePWA({
			registerType: 'autoUpdate',
			workbox: {
				globPatterns: [
					'**/*.{js,css,html,ico,png,svg}'
				],
				cleanupOutdatedCaches: true
			}
		})
	]
});

