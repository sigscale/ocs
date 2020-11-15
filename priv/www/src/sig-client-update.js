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

	_activeItemChanged(item) {
		if(item) {
			this.clientUpAddress = item.id;
			this.clientUpPass = item.secret;
			this.clientUpNewPass = item.newSecret;
			this.clientUpDisPort = item.port;
			this.$.updateClientModal.open();
		} else {
			this.clientUpAddress = null;
			this.clientUpPass = null;
			this.clientUpNewPass = null;
			this.clientUpDisPort = null;
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
		var clientPass = new Array();
		var clientSec = new Object();
		clientSec.op = "replace";
		clientSec.path = "/secret";
		clientSec.value = this.clientUpNewPass;
		var passwordReq = new Object();
		passwordReq.op = "add";
		passwordReq.path = "/passwordRequired";
		if (this.$.updatePass.checked == true) {
			passwordReq.value = false;
		} else {
			passwordReq.value = true;
		}
		if (this.clientUpNewPass == "") {
			clientPass.push(passwordReq);
		} else {
			clientPass.push(clientSec, passwordReq);
		}
		ajax.body = JSON.stringify(clientPass);
		ajax.generateRequest();
		this.$.updatePass.checked = null;
	}

	_updateClientAuthResponse() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_updateClientAuthError() {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateClientProp() {
		var editAjax =  this.$.updateClientPropertiesAjax;
		editAjax.method = "PATCH";
		editAjax.contentType = "application/json-patch+json";
		editAjax.url = "/ocs/v1/client/" + this.$.updateClientId.value;
		var clientArray = new Array();
		var clientPro = new Object();
		clientPro.op = "replace";
		clientPro.path = "/protocol";
		clientPro.value = this.clientUpProto;
		clientArray.push(clientPro);
		var clientPort = new Object();
		clientPort.op = "replace";
		clientPort.path = "/port"
		clientPort.value = parseInt(this.clientUpDisPort);
		clientArray.push(clientPort);
		editAjax.body = JSON.stringify(clientArray);
		editAjax.generateRequest();
	}

	_updateClientPropertiesResponse() {
		this.$.getUpdateClientToast.text = "Updated Properties";
		this.$.getUpdateClientToast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_updateClientPropertiesError() {
		this.$.getUpdateClientToast.text = "Error";
		this.$.getUpdateClientToast.open();
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
		this.$.getUpdateClientToast.text = "Deleted Successfully";
		this.$.getUpdateClientToast.open();
		this.$.updateClientModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('clientList').shadowRoot.getElementById('clientGrid').clearCache();
	}

	_deleteClientError() {
		this.$.getUpdateClientToast.text = "Error";
		this.$.getUpdateClientToast.open();
	}

}

window.customElements.define('sig-client-update', clientUpdate);
