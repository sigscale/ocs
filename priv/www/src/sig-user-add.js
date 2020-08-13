/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import './style-element.js';

class userAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addUserModal" modal>
				<app-toolbar>
					<div main-title>Add User</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="addUserName"
						label="Username"
						value="{{userName}}">
				</paper-input>
				<paper-input
						id="addUserPasword"
						label="Password"
						value="{{passWord}}">
				</paper-input>
				<paper-dropdown-menu
						class="drop"
						label="Language"
						value="{{userLanguage}}">
					<paper-listbox
							id="addUserLocale"
							slot="dropdown-content"
							selected="0">
						<paper-item>English</paper-item>
						<paper-item>Spanish</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_addUserSubmit">
						Submit
					</paper-button>
					<paper-button
							class="cancel-button"
							dialog-dismiss
							on-tap="cancelDialog">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
				id="addUserAjax"
				url="/partyManagement/v1/individual"
				method = "post"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_addUserResponse">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			userName: {
				type: String
			},
			passWord: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	cancelDialog() {
		this.$.addUserModal.close();
		this.userName = null;
		this.passWord = null;
	}

	_addUserSubmit() {
		var ajax = this.$.addUserAjax;
		var user = new Object();
		user.id = this.userName;
		var username = new Object();
		username.name = "username";
		username.value = this.userName;
		var password = new Object();
		password.name = "password";
		password.value = this.passWord;
		var language = new Object();
		language.name = "locale";
		if (this.userLanguage == "English") {
			language.value = "en";
		} else if (this.userLanguage == "Spanish") {
			language.value = "es";
		}
		if(username.value) {
			var characteristic = new Array();
			characteristic.push(username);
			characteristic.push(password);
			characteristic.push(language);
			user.characteristic = characteristic;
			ajax.body = user;
			ajax.generateRequest();
		}
	}

	_addUserResponse() {
		this.$.addUserModal.close();
		this.userName = null;
		this.passWord = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}
}

window.customElements.define('sig-user-add', userAdd);
