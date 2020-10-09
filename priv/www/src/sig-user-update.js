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
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-toast/paper-toast.js';
import '@polymer/paper-item/paper-item.js'
import './style-element.js';

class userUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateUserModal" modal>
				<app-toolbar>
					<div main-title>Update User</div>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="updateUserName"
						name="username"
						label="Username"
						value="{{userUpdateUsername}}"
						disabled>
				</paper-input>
				<paper-input
						id="updateUserPassword"
						name="password"
						value="{{userUpdatePassword}}"
						label="Password">
				</paper-input>
				<paper-dropdown-menu
						class="drop"
						id="updateUserLocale"
						label="Language"
						value="{{userUpLang}}"
						noAnimations="true"
						selected="0">
					<paper-listbox
							slot="dropdown-content">
						<paper-item>English</paper-item>
						<paper-item>Spanish</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
				<div class="buttons">
					<paper-button
							raised
							class="update-button"
							on-tap="_updateUserSubmit">
						Update
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
					<paper-button
							toggles
							raised
							class="delete-button"
							on-tap="_delete">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="updateUserAjax"
					method="PATCH"
					content-type="application/json-patch+json"
					loading="{{loading}}"
					on-response="_updateUserResponse"
					on-error="_updateUserError">
			</iron-ajax>
			<iron-ajax id="deleteUserAjax"
					loading="{{loading}}"
					on-response="_deleteUserResponse"
					on-error="_updateUserError">
			</iron-ajax>
			<paper-toast id="getUpdateUserToast">
			</paper-toast>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			userUpdateUsername: {
				type: String
			},
			userUpdatePassword: {
				type: String
			}
		}
	}

	 ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.userUpdateUsername = item.id;
			this.userUpdatePassword = item.password;
			this.userUpLang = item.language;
			this.$.updateUserModal.open();
		} else {
			this.userUpdateUsername = null;
			this.userUpdatePassword = null;
			this.userUpLang = null;
		}
	}

	_cancel() {
		this.$.updateUserModal.close();
		this.userUpdateUsername = null;
		this.userUpdatePassword = null;
	}

	_updateUserSubmit() {
		var ajax = this.$.updateUserAjax;
		ajax.url = "/partyManagement/v1/individual/" + this.userUpdateUsername;
		var patch = new Array();
		var language;
		if (this.userUpLang == "English") {
			language = "en";
		} else {
			language = "es";
		}
		if (this.userUpLang) {
			var op0 = new Object();
			op0.op = "replace";
			op0.path = "/characteristic/";
			op0.value = new Object();
			op0.value.name = "locale";
			op0.value.value = language;
			patch.push(op0);
		}
		if (this.userUpdatePassword) {
			var op1 = new Object();
			op1.op = "add";
			op1.path = "/characteristic/-";
			op1.value = new Object();
			op1.value.name = "password";
			op1.value.value = this.userUpdatePassword;
			patch.push(op1);
		}
		ajax.body = patch;
		ajax.generateRequest();
	}

	_updateUserResponse() {
		this.$.getUpdateUserToast.text = "Success";
		this.$.getUpdateUserToast.open();
		this.$.updateUserModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}

	_delete() {
		var ajax = this.$.deleteUserAjax;
		ajax.method = "DELETE";
		ajax.url = "/partyManagement/v1/individual/" + document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').selectedItems[0].id;
		ajax.generateRequest();
	}

	_deleteUserResponse() {
		this.$.getUpdateUserToast.text = "Success";
		this.$.getUpdateUserToast.open();
		this.$.updateUserModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}
}

window.customElements.define('sig-user-update', userUpdate);
