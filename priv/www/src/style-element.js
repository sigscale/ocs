const styleElement = document.createElement('dom-module');
styleElement.setAttribute('theme-for', 'vaadin-grid');

styleElement.innerHTML = `<template>
	<style>
		:host {
			@apply(--paper-font-common-base);
			--app-primary-color: var(--paper-yellow-900);
			--app-secondary-color: var(--paper-lime-a700);
			--app-drawer-width: 300px;
			display: block;
		}
		app-header {
			position: fixed;
			top: 0;
			left: 0;
			width: 100%;
			text-align: center;
			background-color: var(--app-primary-color);
			color: #fff;
		}
		paper-toolbar {
			color: white;
			background-color: #bc5100;
		}
		iron-icon {
			padding-right: 10px;
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
		.helpdialog {
			position: fixed;
			min-width: 20em;
			right: -36px;
			top: 41px;
			overflow: auto;
			padding: 0px;
			display: inline-grid;
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
		.drawer-list iron-collapse#tariff {
			padding-left: 40px;
		}
		paper-tabs.details {
			--paper-tabs-selection-bar-color: var(--app-primary-color);
		}
		dt {
			float: left;
			clear: left;
			width: 20ch;
			text-align: right;
			font-weight: bold;
		}
		dt::after {
			content: ":";
		}
		dd {
			margin: 0 0 0 22ch;
		}
		table.details {
			border: 1px solid lightgrey;
			border-collapse: collapse;
		}
		table.details th {
			border: 1px solid lightgrey;
			border-collapse: collapse;
			padding-left: 1em;
			padding-right: 1em;
		}
		table.details td {
			border: 1px solid lightgrey;
			border-collapse: collapse;
			padding-left: 1em;
			padding-right: 1em;
		}
		table.det {
			margin-left: auto;
			margin-right: auto;
		}
		table.det tr td {
			width: 400px;
			word-wrap: break-word;
		}
		table.det tr td {
			display: inline-block;
			margin-top: 2px;
		}
		.yellow-button {
			text-transform: none;
			color: #eeff41;
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
		}
		.add-icon-button {
			color: black;
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
			font-size: medium;
			border-style: none;
			background: #ffb04c;
			max-width: 130px;
		}
		vaadin-grid input::placeholder {
			color: black;
			font-size: medium;
			font-weight: 600;
		}
		vaadin-grid .grouptitle {
			text-align: center;
			border-bottom-style: solid;
			border-color: var(--paper-yellow-900);
		}
		[part~="header-cell"] {
			background-color: #ffb04c;
			font-size: unset;
			font-weight: bolder;
			color: black;
		}
		[part~="body-cell"].expired {
			background-color: var(--paper-red-50);
		}
		[part~="body-cell"].terminal {
			background-color: var(--paper-red-50);
		}
		[part~="body-cell"].correctable {
			background-color: var(--paper-yellow-50);
		}
		div.flowRow {
			display: flex;
		}
		div.flowRow > paper-input {
			flex-grow: 1;
		}
		vaadin-grid-cell-content > div.buttons > paper-button {
			font-size: 0.7rem;
			padding: unset;
		}
		paper-fab {
			background: var(--paper-lime-a700);
			color: black;
		}
		paper-checkbox {
			--paper-checkbox-checked-color: #ffb04c;
			--paper-checkbox-checkmark-color: var(--paper-yellow-900);
			padding-right: 1em;
		}
		.generate {
			display: inline-block;
			width: 8em;
		}
		.passwordless {
			width: 8em;
		}
		.trusted {
			display: block;
			padding-bottom: 5px;
			padding-block-start: 5px;
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
		sig-dashboard {
			display: flex;
			flex-wrap: wrap;
			flex-direction: row;
			align-items: flex-start;
			gap: 8px;
		}
		paper-card {
			--paper-card: {
				border-radius: 8px;
			};
		}
		paper-icon-button.hide-button {
			float: right;
		}
		#schedCard {
			flex-direction: row;
			flex: 1 0 100%;
			order: 1;
		}
		#schedCard div.card-content svg {
			width: 100%;
		}
		#diaCard {
			flex-direction: row;
			flex: 1 0 100%;
			order: 2;
		}
		#diaCard div.card-content svg {
			width: 100%;
		}
		#subsCard {
			flex-direction: row;
			order: 3;
		}
		#creditCard {
			flex-direction: row;
			order: 4;
		}
		#upCard {
			flex-direction: row;
			order: 5;
		}
		#staCard {
			flex-direction: row;
			order: 4;
		}
	</style>
</template>`;

styleElement.register('style-element');
