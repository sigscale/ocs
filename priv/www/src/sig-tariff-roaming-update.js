/**
 * Copyright 2016 - 2022 SigScale Global Inc.
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

class roamingUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateRoamingModal" modal>
				<app-toolbar>
					<h2>Update Roaming</h2>
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
						allowed-pattern="[+0-9]"
						pattern="^[+]?[0-9]+"
						auto-validate
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
						allowed-pattern="[0-9.]"
						pattern="^[0-9]+\.?[0-9]{0,6}$"
						auto-validate
						label="Rate"
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
			<iron-ajax id="updateRoamingRowAjax"
					on-response="_updateRoamingRowResponse"
					on-error="_updateRoamingRowError"
					on-loading-changed="_onLoadingChanged">
			</iron-ajax>
			<iron-ajax id="deleteRoamingRowAjax"
				on-response="_deleteRoamingRowResponse"
				on-error="_deleteRoamingRowError">
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
			storeRoaming: {
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
			document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').rowId = current.id;
			this.upPrefix = current.prefix;
			this.upDesc = current.description;
			this.upRate = current.rate;
			var arrObj = new Object();
			arrObj.prefix = this.upPrefix;
			arrObj.description = this.upDesc;
			arrObj.rate = this.upRate;
			this.storeRoaming.push(arrObj);
			this.$.updateRoamingModal.open();
		}
	}

	UpdateButton(event) {
		var Ajax = this.$.updateRoamingRowAjax;
		Ajax.method = "PATCH";
		Ajax.contentType = "application/json-patch+json";
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').activeTableName;
		var Id = this.upPrefix;
		Ajax.url = "/resourceInventoryManagement/v1/resource/" + Table + "-" + Id;
		var ResArray = new Array();
		for(var indexx in this.storeRoaming) {
			if(this.upDesc != this.storeRoaming[indexx].description) {
				var Desc = new Object(); 
				Desc.op = "replace";
				Desc.path = "/resourceCharacteristic/1/value";
				Desc.value = this.upDesc;
				ResArray.push(Desc);
			}
			if(this.upRate != this.storeRoaming[indexx].rate) {
				var Rate = new Object();
				Rate.op = "replace";
				Rate.path = "/resourceCharacteristic/2/value";
				Rate.value = this.upRate;
				ResArray.push(Rate);
			}
		}
		Ajax.body = JSON.stringify(ResArray);
		Ajax.generateRequest();
		this.$.upPrefix = null;
		this.$.upDesc = null;
		this.$.upRate = null;
	}

	_updateRoamingRowResponse(event) {
		this.$.updateRoamingModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid').clearCache();
	}

	_updateRoamingRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	deleteUpdate(event) {
		var Table = document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid');
		var Id = this.upPrefix;
		this.$.deleteRoamingRowAjax.method = "DELETE";
		this.$.deleteRoamingRowAjax.url = "/resourceInventoryManagement/v1/resource/" + Table.rowId;
		this.$.deleteRoamingRowAjax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid').clearCache();
	}

	_deleteRoamingRowResponse(event) {
		this.$.updateRoamingModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid').clearCache();
	}

	_deleteRoamingRowError(event) {
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
		if (this.$.updateRoamingRowAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-roaming-update', roamingUpdate);
