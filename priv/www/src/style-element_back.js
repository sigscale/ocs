const styleElement = document.createElement('dom-module');
styleElement.setAttribute('theme-for', 'vaadin-grid');

styleElement.innerHTML = `<template>
		<style>
			:host {
            @apply(--paper-font-common-base);
            --app-primary-color: #f57f17;
            --app-secondary-color: #aeea00;
            display: block;
         }
         .toolbar-top {
            background-color: var(--app-primary-color);
         }
         app-header {
            position: fixed;
            top: 0;
            left: 0;
            width: 100%;
            text-align: center;
            background-color: var(--app-primary-color);
            border-bottom: 1px solid #eee;
            color: #fff;
         }
         app-header paper-icon-button {
            --paper-icon-button-ink-color: white;
         }
			app-toolbar {
				background: var(--paper-yellow-900);
			}
			app-drawer {
				--app-drawer-content-container: {
					padding-top: 10px;
				};
				height: 100%;
				top: 64px;
			}
			paper-dialog {
				overflow: auto;
			}
			paper-progress {
				display: block;
				width: 100%;
				--paper-progress-active-color: var(--paper-lime-a700);
				--paper-progress-container-color: transparent;
			}
			iron-pages {
				height: 100%;
			}
			iron-icon {
				padding-right: 10px;
			}
			.icon-style {
				min-height: 10px;
			}
			.add-button{
				color: black;
			}
			.ok-button {
				background-color: var(--paper-lime-a700);
				color: black;
				width: 8em;
			}
			.cancel-button {
				color: black;
			}
			.delete-buttons {
				background: #EF5350;
				color: black;
			}
			.sublist paper-icon-item {
				padding-left: 30px;
			}
			.sublistipdr paper-icon-item {
				padding-left: 60px;
			}
		</style>
   </template>`;

styleElement.register('style-element');
