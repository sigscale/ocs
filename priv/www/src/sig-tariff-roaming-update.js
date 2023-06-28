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
				<div>
					<paper-input
							id="updatePrefix"
							name="Prefix"
							allowed-pattern="[+0-9]"
							pattern="^[+]?[0-9]+"
							auto-validate
							label="Prefix"
							value="{{roamingRowPrefix}}"
							disabled>
					</paper-input>
					<paper-tooltip
							for="updatePrefix"
							offset="0">
						Prefix to match
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateDescription"
							name="Description"
							label="Description"
							value="{{roamingRowDescription}}">
					</paper-input>
					<paper-tooltip
							for="updateDescription"
							offset="0">
						Description of the prefix
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateTariff"
							name="tariffUpdate"
							label="Tariff Table Name"
							value="{{roamingRowTariff}}">
					</paper-input>
					<paper-tooltip
							for="updateTariff"
							offset="0">
						Name of the roaming tariff table applied for the prefix
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="update-button"
							on-tap="_updateRoamingRow">
						Update
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							dialog-dismiss
							on-tap="_cancelRoamingRow">
						Cancel
					</paper-button>
					<paper-button toggles
							raised
							on-tap="_deleteRoamingRow"
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
			roamingRowId: {
				type:String
			},
			roamingRowPrefix: {
				type: String
			},
			roamingRowDescription: {
				type: String
			},
			roamingRowTariff: {
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
			this.roamingRowId = current.id;
			this.roamingRowPrefix = current.prefix;
			this.roamingRowDescription = current.description;
			this.roamingRowTariff = current.rate;
			this.$.updateRoamingModal.open();
		}
	}

	_updateRoamingRow(event) {
		var ajax = this.$.updateRoamingRowAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		var table = document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').activeTableName;
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.roamingRowId;
		var patch = new Array();
		function isDescription(element) {
			if(element.name == "description") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isDescription);
		if(index === -1) {
			var description = new Object();
			description.op = "add";
			description.path = "/resourceCharacteristic/-";
			description.value = {name: "description", value: this.roamingRowDescription};
			patch.push(description);
		} else {
			var description = new Object();
			description.op = "replace";
			description.path = "/resourceCharacteristic/" + index + "/value";
			description.value = this.roamingRowDescription;
			patch.push(description);
		}
		function isTariff(element) {
			if(element.name == "tariff") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isTariff);
		if(index === -1) {
			var tariff = new Object();
			tariff.op = "add";
			tariff.path = "/resourceCharacteristic/-";
			tariff.value = {name: "tariff", value: this.roamingRowTariff};
			patch.push(tariff);
		} else {
			var tariff = new Object();
			tariff.op = "replace";
			tariff.path = "/resourceCharacteristic/" + index + "/value";
			tariff.value = this.roamingRowTariff/
			patch.push(tariff);
		}
		ajax.body = JSON.stringify(patch);
		ajax.generateRequest();
		this.$.roamingRowId = null;
		this.$.roamingRowPrefix = null;
		this.$.roamingRowDescription = null;
		this.$.roamingRowTariff = null;
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

	_deleteRoamingRow(event) {
		var ajax = this.$.deleteRoamingRowAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.$.roamingRowPrefix;
		ajax.generateRequest();
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

	_cancelRoamingRow() {
		this.$.roamingRowId = null;
		this.$.roamingRowPrefix = null;
		this.$.roamingRowDescription = null;
		this.$.roamingRowTariff = null;
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
