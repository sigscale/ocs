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
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class clientUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateClientModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
						<paper-tab id="authenticate-client">
							<h2>Authentication</h2>
						</paper-tab>
						<paper-tab id="authorize-client">
							<h2>Properties</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div>
						<paper-input
								id="updateClientId"
								name="id"
								label="Address"
								value="{{clientUpAddress}}"
								disabled>
						</paper-input>
						<paper-input
								id="updateClientPassword"
								name="password"
								label="Secret"
								value="{{clientUpPass}}"
								disabled>
						</paper-input>
						<paper-input
								id="updateClientNewPassword"
								name="secret"
								label="New Password"
								value="{{clientUpNewPass}}">
						</paper-input>
						<paper-tooltip
								for="updateClientNewPassword"
								offset="0">
							New client password
						</paper-tooltip>
						<paper-checkbox
								class="passwordless"
								value="{{updatePasswordless}}"
								id="updatePass">
							Passwordless
						</paper-checkbox>
						<div class="buttons">
							<paper-button
									raised
									class="update-button"
									on-tap="updateClientAuth">
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
									on-tap="deleteClient">
								Delete
							</paper-button>
						</div>
					</div>
					<div id="edit-client-attributes">
						<paper-dropdown-menu
								class="drop"
								id="updateClientProtocol"
								label="Protocol"
								value="{{clientUpProto}}"
								no-animations="true"
								on-selected-item-changed="checkProto">
							<paper-listbox
									id="updateClientProtocolList"
									slot="dropdown-content"
									selected="0">
								<paper-item>RADIUS</paper-item>
								<paper-item>DIAMETER</paper-item>
							</paper-listbox>
						</paper-dropdown-menu>
						<paper-tooltip
								for="updateClientProtocol"
								offset="0">
							Choose the protocol from dropdown list
						</paper-tooltip>
						<paper-input
								id="updateClientDisconnectPort"
								name="Port"
								label="Port"
								type="number"
								value="{{clientUpDisPort}}">
						</paper-input>
						<paper-tooltip
								for="updateClientDisconnectPort"
								offset="0">
							New disconnectPort
						</paper-tooltip>
						<div class="buttons">
							<paper-button
									raised
									class="update-button"
									on-tap="updateClientProp">
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
									on-tap="deleteClientProp">
								Delete
							</paper-button>
						</div>
					</div>
				</iron-pages>
			</paper-dialog>
			<iron-ajax id="updateClientAuthAjax"
				on-response="_updateClientAuthResponse"
				on-error="_updateClientAuthError">
			</iron-ajax>
			<iron-ajax id="updateClientPropertiesAjax"
				on-response="_updateClientPropertiesResponse"
				on-error="_updateClientPropertiesError">
			</iron-ajax>
			<iron-ajax id="deleteClientAjax"
				on-response="_deleteClientResponse"
				on-error="_deleteClientError">
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
			valCha: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			valChaOne: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			selected: {
				type: Number,
				value: 0
			},
			clientUpAddress: {
				type: String
			},
			clientUpPass: {
				type: String
			},
			clientUpNewPass: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var current;
			if(item) {
				current = item;
			} else if(last) {
				current = last;
			}
			this.clientUpAddress = current.id;
			this.clientUpPass = current.secret;
			this.clientUpNewPass = current.newSecret;
			this.clientUpProto = current.protocol;
			this.clientUpDisPort = current.port;
			this.updatePasswordless = current.passwordRequired;
			var arrObj = new Object();
			arrObj.port = this.clientUpDisPort;
			arrObj.protocol = this.clientUpProto;
			arrObj.address = this.clientUpAddress;
			arrObj.pass = this.clientUpNewPass;
			arrObj.passwordless = this.updatePasswordless;
			this.valCha.push(arrObj);
			this.$.updateClientModal.open();
		}
	}

	_cancel() {
		this.$.updateClientModal.close();
		this.clientUpAddress = null;
		this.clientUpPass = null;
		this.clientUpNewPass = null;
		this.clientUpDisPort = null;
	}

	updateClientAuth() {
		var ajax = this.$.updateClientAuthAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/ocs/v1/client/" + this.clientUpAddress;
		for(var indexx in this.valCha) {
			var penAuth = new Set();
			var penAuthObj = new Object();
			var penAuthObj1 = new Object(); 
			if(this.clientUpNewPass != this.valCha[indexx].pass){
				penAuthObj.pass = this.clientUpNewPass;
				penAuth.add(this.clientUpNewPass);
			}
			if(this.updatePasswordless != this.valCha[indexx].passwordless){
				penAuthObj.passwordless = this.updatePasswordless;
				penAuth.add(this.updatePasswordless);
			}
			if (penAuth.has(this.clientUpNewPass)) {
				penAuthObj1.pass = this.clientUpNewPass;
			}
			if (penAuth.has(this.updatePasswordless)) {
				penAuthObj1.passwordless = this.updatePasswordless;
			}
			this.valChaOne.push(penAuthObj1);
		}
		for(var indexx1 in this.valChaOne) {
			var clientPass = new Array();
			if(this.valChaOne[indexx1].pass) {
				var clientSec = new Object();
				clientSec.op = "replace";
				clientSec.path = "/secret";
				clientSec.value = this.valChaOne[indexx1].pass;
				clientPass.push(clientSec);
			}
			if(this.valChaOne[indexx1].passwordless) {
				var passwordReq = new Object();
				passwordReq.op = "add";
				passwordReq.path = "/passwordRequired";
				passwordReq.value = this.valChaOne[indexx1].passwordless;
				clientPass.push(passwordReq);
			}
		}
		ajax.body = JSON.stringify(clientPass);
		ajax.generateRequest();
		this.$.updatePass.checked = null;
	}

	_updateClientAuthResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		this.$.updateClientModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_updateClientAuthError() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateClientProp(event) {
		var editAjax =  this.$.updateClientPropertiesAjax;
		editAjax.method = "PATCH";
		editAjax.contentType = "application/json-patch+json";
		editAjax.url = "/ocs/v1/client/" + this.$.updateClientId.value;
		for(var indexx in this.valCha) {
			var pen = new Set();
			var penObj = new Object();
			var penObj1 = new Object(); 
			if(this.clientUpDisPort != this.valCha[indexx].port){
				penObj.port = this.clientUpDisPort;
				pen.add(this.clientUpDisPort);
			}
			if(this.clientUpProto != this.valCha[indexx].protocol){
				penObj.protocol = this.clientUpProto;
				pen.add(this.clientUpProto);
			}
			if (pen.has(this.clientUpDisPort)) {
				penObj1.port = this.clientUpDisPort;
			}
			if (pen.has(this.clientUpProto)) {
				penObj1.protocol = this.clientUpProto;
			}
			this.valChaOne.push(penObj1);
		}
		for(var indexx1 in this.valChaOne) {
			var clientArray = new Array();
			if(this.valChaOne[indexx1].protocol) {
				var clientPro = new Object();
				clientPro.op = "replace";
				clientPro.path = "/protocol";
				clientPro.value = this.valChaOne[indexx1].protocol;
				clientArray.push(clientPro);
			}
			if(this.valChaOne[indexx1].port) {
				var clientPort = new Object();
				clientPort.op = "replace";
				clientPort.path = "/port"
				clientPort.value = parseInt(this.valChaOne[indexx1].port);
				clientArray.push(clientPort);
			}
		}
		editAjax.body = JSON.stringify(clientArray);
		editAjax.generateRequest();
	}

	_updateClientPropertiesResponse() {
		var getUpdateClientToast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		getUpdateClientToast.text = "Updated Properties";
		getUpdateClientToast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_updateClientPropertiesError() {
		var getUpdateClientToast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		getUpdateClientToast.text = "Error";
		getUpdateClientToast.open();
	}

	checkProto() {
		if(this.clientUpProto == "RADIUS") {
			this.$.updateClientDisconnectPort.disabled = false;
		} else if(this.clientUpProto == "DIAMETER") {
			this.$.updateClientDisconnectPort.disabled = true;
		}
	}

	deleteClient() {
		this.$.deleteClientAjax.method = "DELETE";
		this.$.deleteClientAjax.url = "/ocs/v1/client/" + document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').selectedItems[0].id;
		this.$.deleteClientAjax.generateRequest();
	}

	_deleteClientResponse() {
		var getUpdateClientToast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		getUpdateClientToast.text = "Deleted Successfully";
		getUpdateClientToast.open();
		this.$.updateClientModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_deleteClientError() {
		var getUpdateClientToast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		getUpdateClientToast.text = "Error";
		getUpdateClientToast.open();
	}
}

window.customElements.define('sig-client-update', clientUpdate);
