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
import '@polymer/paper-toast/paper-toast.js';
import './style-element.js';

class tableUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updatePrefixModal" modal>
				<app-toolbar>
					<h2>Update Prefix</h2>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="updatePrefix"
						name="Prefix"
						label="Prefix"
						value="{{upPrefix}}"
						disabled>
				</paper-input>
				<paper-input
						id="updateDescription"
						name="Description"
						label="Description"
						value="{{upDesc}}">
				</paper-input>
				<paper-input
						id="updateRate"
						name="rateUpdate"
						label="Rate"
						type="number"
						value="{{upRate}}">
				</paper-input>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="update-button"
							on-tap="UpdateButton">
						Update
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							dialog-dismiss
							on-tap="cancelUpdate">
						Cancel
					</paper-button>
					<paper-button toggles
							raised
							on-tap="delete-button"
							class="delete-buttons">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<paper-toast
					id="updateTableRowToastError">
			</paper-toast>
			<paper-toast
					id="deleteTableRowToastError">
			</paper-toast>
			<iron-ajax id="updateTableRowAjax"
					on-response="_updateTableRowResponse"
					on-error="_updateTableRowError"
					on-loading-changed="_onLoadingChanged">
			</iron-ajax>
			<iron-ajax id="deleteTableRowAjax"
				on-response="_deleteTableRowResponse"
				on-error="_deleteTableRowError">
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
			upPrefix: {
				type: String
			},
			upDesc: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if(item) {
			this.upPrefix = item.prefix;
			this.upDesc = item.description;
			this.upRate = item.rate;
			this.$.updatePrefixModal.open();
		} else {
			this.upPrefix = null;
			this.upDesc = null;
			this.upRate = null;
		}
	}

	UpdateButton(event) {
		var Ajax = this.$.updateTableRowAjax;
		Ajax.method = "PATCH";
		Ajax.contentType = "application/json-patch+json";
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').table;
		var Id = this.$.upPrefix;
		Ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + Table + "/" + Id;
		var ResArray = new Array();
		var Desc = new Object(); 
		Desc.op = "add";
		Desc.path = "/resourceCharacteristic/1/value/value";
		Desc.value = this.$.upDesc;
		ResArray.push(Desc);
		var Rate = new Object();
		Rate.op = "add";
		Rate.path = "/resourceCharacteristic/2/value/value";
		Rate.value = this.$.upRate;
		ResArray.push(Rate);
		Ajax.body = JSON.stringify(ResArray);
		Ajax.generateRequest();
		this.$.upPrefix = null;
		this.$.upDesc = null;
		this.$.upRate = null;
	}

	_updateTableRowResponse(event) {
		this.$.updatePrefixModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_updateTableRowError(event) {
		this.$.updateTableRowToastError.text = event.detail.request.xhr.statusText;
		this.$.updateTableRowToastError.open();
	}

	deleteUpdate(event) {
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').table;
		var Id = this.upPrefix;
		this.$.deleteTableRowAjax.method = "DELETE";
		this.$.deleteTableRowAjax.url = "/resourceInventoryManagement/v1/logicalResource/"
				+ Table + "/" + Id;
		this.$.deleteTableRowAjax.generateRequest();
	}

	_deleteTableRowResponse(event) {
		this.$.updatePrefixModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_deleteTableRowError(event) {
		this.$.deleteTableRowToastError.text = event.detail.request.xhr.statusText;
		this.$.deleteTableRowToastError.open();
	}

	cancelUpdate() {
		this.$.upPrefix = null;
		this.$.upDesc = null;
		this.$.upRate = null;
	}
}

window.customElements.define('sig-prefix-update', tableUpdate);
