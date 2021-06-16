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
import '@polymer/paper-tooltip/paper-tooltip.js';
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
						id="progressId"
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
				<paper-tooltip
						for="updatePrefix"
						offset="0">
					Value to update prefix number
				</paper-tooltip>
				<paper-input
						id="updateDescription"
						name="Description"
						label="Description"
						value="{{upDesc}}">
				</paper-input>
				<paper-tooltip
						for="updateDescription"
						offset="0">
					Value to update prefix description
				</paper-tooltip>
				<paper-input
						id="updateRate"
						name="rateUpdate"
						label="Rate"
						type="number"
						value="{{upRate}}">
				</paper-input>
				<paper-tooltip
						for="updateRate"
						offset="0">
					Value to update prefix rate
				</paper-tooltip>
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
							on-tap="deleteUpdate"
							class="delete-button">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
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
			storeTariff: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
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

	_activeItemChanged(item, last) {
		if(item || last) {
			var current;
			if(item) {
				current = item;
			} else if(last) {
				current = last;
			}
			document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').rowId = current.id;
			this.upPrefix = current.prefix;
			this.upDesc = current.description;
			this.upRate = current.rate;
			var arrObj = new Object();
			arrObj.prefix = this.upPrefix;
			arrObj.description = this.upDesc;
			arrObj.rate = this.upRate;
			this.storeTariff.push(arrObj);
			this.$.updatePrefixModal.open();
		}
	}

	UpdateButton(event) {
		var Ajax = this.$.updateTableRowAjax;
		Ajax.method = "PATCH";
		Ajax.contentType = "application/json-patch+json";
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').activeTableName;
		var Id = this.upPrefix;
		Ajax.url = "/resourceInventoryManagement/v1/resource/" + Table + "-" + Id;
		var ResArray = new Array();
		for(var indexx in this.storeTariff) {
			if(this.upDesc != this.storeTariff[indexx].description) {
				var Desc = new Object(); 
				Desc.op = "replace";
				Desc.path = "/resourceCharacteristic/1/value";
				Desc.value = this.upDesc;
				ResArray.push(Desc);
			}
			if(this.upRate != this.storeTariff[indexx].rate) {
				var Rate = new Object();
				Rate.op = "replace";
				Rate.path = "/resourceCharacteristic/2/value";
				Rate.value = parseInt(this.upRate);
				ResArray.push(Rate);
			}
		}
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
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	deleteUpdate(event) {
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList');
		var Id = this.upPrefix;
		this.$.deleteTableRowAjax.method = "DELETE";
		this.$.deleteTableRowAjax.url = "/resourceInventoryManagement/v1/resource/" + Table.rowId;;
		this.$.deleteTableRowAjax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_deleteTableRowResponse(event) {
		this.$.updatePrefixModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_deleteTableRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	cancelUpdate() {
		this.$.upPrefix = null;
		this.$.upDesc = null;
		this.$.upRate = null;
	}

	_onLoadingChanged(event) {
		if (this.$.updateTableRowAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-prefix-update', tableUpdate);
