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
		.toolbar-top {
			background-color: var(--app-primary-color);
		}
		app-header paper-icon-button {
			--paper-icon-button-ink-color: white;
		}
		app-drawer {
			--app-drawer-content-container: {
				padding-top: 10px;
			};
			height: 100%;
			top: 64px;
		}
		.drawer-list {
			box-sizing: border-box;
			width: 100%;
			height: 100%;
			padding: 10px;
			background: white;
			position: relative;
		}
		.drawer-list a {
			display: block;
			padding: 0 24px;
			text-decoration: none;
			color: black;
			line-height: 40px;
		}
		.dialog {
			overflow: auto;
			max-width: 60%;
			max-height: 80%;
		}
		paper-dialog app-toolbar {
			margin-top: 0px;
			color: white;
			background-color: #bc5100;
		}
		paper-dialog > *:first-child {
			margin-top: 0px;
		}
		paper-dialog iron-collapse {
			--paper-input-container-underline: {
				display: none;
			};
		}
		paper-dialog iron-collapse > div hr {
			border-top: 1px solid blue;
		}
		paper-dialog iron-collapse > div:first-child hr {
			display: none;
		}
		paper-dialog iron-collapse > div {
			padding-top: 25px
		}
		paper-dialog iron-collapse > div:first-child {
			padding-top: 0px
		}
		.drawer-list a.iron-selected {
			color: #78909C;
			font-weight: bold;
		}
		.drawer-list iron-collapse#catalog {
			padding-left: 36px;
		}
		.drawer-list iron-collapse#subscribers {
			padding-left: 36px;
		}
		.drawer-list iron-collapse#logs {
			padding-left: 36px;
		}
		.drawer-list iron-collapse#ipdr {
			padding-left: 36px;
		}
		.add-button {
			right: 2%;
			position: fixed;
			bottom: 5%;
			z-index: 100;
		}
		.submit-button {
			background-color: var(--paper-lime-a700);
			color: black;
			float: right;
			width: 8em;
		}
		.update-button {
			background-color: var(--paper-lime-a700);
			color: black;
		}
		.cancel-button {
			color: black;
		}
		.delete-button {
			background: #EF5350;
			color: black;
		}
		paper-progress {
			display: block;
			width: 100%;
			margin: 0px;
			padding: 0px;
			--paper-progress-active-color: var(--paper-lime-a700);
			--paper-progress-container-color: transparent;
		}
		vaadin-grid {
			height: 100vh;
			font-size: inherit;
		}
		vaadin-grid input {
			font-size: initial;
			border-style: none;
			background: #ffb04c;
			max-width: 130px;
		}
		vaadin-grid input::placeholder {
			color: black;
			font-weight: bold;
			font-size: inherit;
		}
		[part="row"]:last-child [part~="header-cell"] {
			background-color: #ffb04c;
		}
		paper-fab {
			background: var(--paper-lime-a700);
			color: black;
		}
			paper-checkbox {
				--paper-checkbox-checked-color: #ffb04c;
				--paper-checkbox-checkmark-color: var(--paper-yellow-900);
			}
			.generate {
				display: inline-block;
				width: 8em;
			}
			.passwordless {
				width: 8em;
				padding-left: 10px;
			}
			.secret {
				display: inline-block;
			}
			.generated {
				padding: 10px;
				overflow: auto;
			}
			.close {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 5em;
			}
			paper-item-body {
				padding-left: 80px;
				--paper-item-body-secondary: {
					font-weight: bold;
					font-size: larger;
				}
			}
		</style>
	</template>`;

styleElement.register('style-element');
