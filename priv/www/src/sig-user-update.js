/**
 * Copyright 2016 - 2021 SigScale Global Inc.
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
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-tooltip/paper-tooltip.js';
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
				<paper-tooltip
						for="updateUserName"
						offset="0">
					Add a value to update username
				</paper-tooltip>
				<paper-input
						id="updateUserPassword"
						name="password"
						value="{{userUpdatePassword}}"
						label="Password">
				</paper-input>
				<paper-tooltip
						for="updateUserPassword"
						offset="0">
					Add a value to update password
				</paper-tooltip>
				<paper-dropdown-menu
						class="drop"
						id="updateUserLocale"
						label="Language"
						value="{{userUpLang}}"
						no-animations="true"
						selected="0">
					<paper-listbox
							slot="dropdown-content">
						<paper-item>English</paper-item>
						<paper-item>Spanish</paper-item>
					</paper-listbox>
				</paper-dropdown-menu>
				<paper-tooltip
						for="updateUserLocale"
						offset="0">
					Select a value to update user language
				</paper-tooltip>
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
			<iron-ajax
				id="getUserAjaxUp"
				method="GET">
			</iron-ajax>
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
			valChaUser: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			valChaOneUser: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
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
			var ajaxCh = this.$.getUserAjaxUp;
			ajaxCh.url = "/partyManagement/v1/individual/" + item.id;
			ajaxCh.generateRequest();
			this.userUpdateUsername = item.id;
			this.userUpdatePassword = item.password;
			this.userUpLang = item.language;
			var arrObjUser = new Object();
			arrObjUser.username = this.userUpdateUsername;
			arrObjUser.password = this.userUpdatePassword;
			arrObjUser.lang = this.userUpLang;
			this.valChaUser.push(arrObjUser);
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
		var getAjax = this.$.getUserAjaxUp;
		var results = getAjax.lastResponse;
		var ajax = this.$.updateUserAjax;
		ajax.url = "/partyManagement/v1/individual/" + this.userUpdateUsername;
		for(var indUser in this.valChaUser) {
			var penUser = new Set();
			var penObjUser = new Object();
			var penObj1User = new Object();
			if(this.userUpdatePassword != this.valChaUser[indUser].password){
				penObjUser.password = this.userUpdatePassword;
				penUser.add(this.userUpdatePassword);
			}
			if(this.userUpLang != this.valChaUser[indUser].lang){
				penObjUser.lang = this.userUpLang;
				penUser.add(this.userUpLang);
			}
			if (penUser.has(this.userUpdatePassword)) {
				penObj1User.password = this.userUpdatePassword;
			}
			if (penUser.has(this.userUpLang)) {
				penObj1User.lang = this.userUpLang;
			}
			this.valChaOneUser.push(penObj1User);
		}
		for(var indUserOne in this.valChaOneUser) {
			var userArray = new Array();
			function checkLoc(chara) {
				return chara.name == "locale";
			}
			var index = results.characteristic.findIndex(checkLoc);
			var language;
			if(this.valChaOneUser[indUserOne].lang) {
				if (this.valChaOneUser[indUserOne].lang == "English") {
					language = "en";
				} else {
					language = "es";
				}
			}
			if(this.valChaOneUser[indUserOne].lang) {
				var op0 = new Object();
				op0.op = "replace";
				op0.path = "/characteristic/" + index;
				var charlocal = new Object();
				charlocal.name = "locale";
				charlocal.value = language;
				op0.value = charlocal;
				userArray.push(op0);
			}
			if(this.valChaOneUser[indUserOne].password) {
				var op1 = new Object();
				op1.op = "add";
				op1.path = "/characteristic/-";
				var charPass = new Object();
				charPass.name = "password";
				charPass.value = this.valChaOneUser[indUserOne].password;
				op1.value = charPass;
				userArray.push(op1);
			}
		}
		ajax.body = JSON.stringify(userArray);
		ajax.generateRequest();
	}

	_updateUserResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
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
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.$.updateUserModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('userList').shadowRoot.getElementById('userGrid').clearCache();
	}
}

window.customElements.define('sig-user-update', userUpdate);
