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

class rateUpdate extends PolymerElement {
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
				<div>
				</div>
					<paper-input
							id="updatePrefix"
							name="Prefix"
							allowed-pattern="[+0-9]"
							pattern="^[+]?[0-9]+"
							auto-validate
							label="Prefix"
							value="{{rateRowPrefix}}"
							disabled>
					</paper-input>
					<paper-tooltip
							for="updatePrefix"
							offset="0">
						Prefix to match
					</paper-tooltip>
					<paper-input
							id="updateDescription"
							name="Description"
							label="Description"
							value="{{rateRowDescription}}">
					</paper-input>
					<paper-tooltip
							for="updateDescription"
							offset="0">
						Description of the prefix
					</paper-tooltip>
				<div>
				</div>
					<paper-input
							id="updateRate"
							name="rateUpdate"
							allowed-pattern="[0-9.]"
							pattern="^[0-9]+\.?[0-9]{0,6}$"
							auto-validate
							label="Rate"
							value="{{rateRowRate}}">
					</paper-input>
					<paper-tooltip
							for="updateRate"
							offset="0">
						Rate applied for the prefix match
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="update-button"
							on-tap="_updateRateRow">
						Update
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							dialog-dismiss
							on-tap="_cancelRateRow">
						Cancel
					</paper-button>
					<paper-button toggles
							raised
							on-tap="_deleteRateRow"
							class="delete-button">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax id="updateRateRowAjax"
					on-response="_updateRateRowResponse"
					on-error="_updateRateRowError"
					on-loading-changed="_onLoadingChanged">
			</iron-ajax>
			<iron-ajax id="deleteRateRowAjax"
				on-response="_deleteRateRowResponse"
				on-error="_deleteRateRowError">
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
			rateRowId: {
				type: String
			},
			rateRowPrefix: {
				type: String
			},
			rateRowDescription: {
				type: String
			},
			rateRowrate: {
				type: Number
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
			this.rateRowId = current.id;
			this.rateRowPrefix = current.prefix;
			this.rateRowDescription = current.description;
			this.rateRowRate = current.rate;
			this.$.updatePrefixModal.open();
		}
	}

	_updateRateRow(event) {
		var ajax = this.$.updateRateRowAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.rateRowId;
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
			description.value = {name: "description", value: this.rateRowDescription};
			patch.push(description);
		} else if(this.rateRowDescription != this.activeItem.resourceCharacteristic[index].value) {
			var description = new Object();
			description.op = "replace";
			description.path = "/resourceCharacteristic/" + index + "/value";
			description.value = this.rateRowDescription;
			patch.push(description);
		}
		function isRate(element) {
			if(element.name == "rate") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isRate);
		if(index === -1) {
			var rate = new Object();
			rate.op = "add";
			rate.path = "/resourceCharacteristic/-";
			rate.value = {name: "rate", value: this.rateRowRate};
			patch.push(rate);
		} else if(this.rateRowRate != this.activeItem.resourceCharacteristic[index].value) {
			var rate = new Object();
			rate.op = "replace";
			rate.path = "/resourceCharacteristic/" + index + "/value";
			rate.value = this.rateRowRate;
			patch.push(rate);
		}
		ajax.body = JSON.stringify(patch);
		ajax.generateRequest();
	}

	_updateRateRowResponse(event) {
		this.rateRowId = null;
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
		this.$.updatePrefixModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('rateList').shadowRoot.getElementById('rateGrid').clearCache();
	}

	_updateRateRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_deleteRateRow(event) {
		var ajax = this.$.deleteRateRowAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.rateRowId;
		ajax.generateRequest();
		this.rateRowId = null;
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('rateList').shadowRoot.getElementById('rateGrid').clearCache();
	}

	_deleteRateRowResponse(event) {
		this.$.updatePrefixModal.close();
		this.rateRowId = null;
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('rateList').shadowRoot.getElementById('rateGrid').clearCache();
	}

	_deleteRateRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancelRateRow() {
		this.rateRowId = null;
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
	}

	_onLoadingChanged(event) {
		if (this.$.updateRateRowAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-rate-update', rateUpdate);
