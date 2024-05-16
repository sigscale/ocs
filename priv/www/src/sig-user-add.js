/**
 * Copyright 2016 - 2024 SigScale Global Inc.
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
import '@polymer/paper-tooltip/paper-tooltip.js';
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
				<paper-tooltip
						for="addUserName"
						offset="0">
					User name
				</paper-tooltip>
				<paper-input
						id="addUserPasword"
						label="Password"
						value="{{passWord}}">
				</paper-input>
				<paper-tooltip
						for="addUserPasword"
						offset="0">
					User password
				</paper-tooltip>
				<paper-dropdown-menu
						id="addUserLoc"
						class="drop"
						label="Language"
						no-animations="true"
						value="{{userLanguage}}">
					<paper-listbox
							id="addUserLocale"
							slot="dropdown-content"
							selected="0">
						<paper-item>English</paper-item>
						<paper-item>Spanish</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
				<paper-tooltip
						for="addUserLoc"
						offset="0">
					Select user language from list
				</paper-tooltip>
				<paper-input
						id="addUserRate"
						label="Rating"
						value="{{rating}}">
				</paper-input>
				<paper-tooltip
						for="addUserRate"
						offset="0">
					Rating Role
				</paper-tooltip>

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
		var ratingRole = new Object();
		ratingRole.name = "rating";
		ratingRole.value = Boolean(this.rating);
		if(username.value) {
			var characteristic = new Array();
			characteristic.push(username);
			characteristic.push(password);
			characteristic.push(language);
			characteristic.push(ratingRole);
			user.characteristic = characteristic;
			ajax.body = user;
			ajax.generateRequest();
		}
	}

	_addUserResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.$.addUserModal.close();
		this.userName = null;
		this.passWord = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}
}

window.customElements.define('sig-user-add', userAdd);
