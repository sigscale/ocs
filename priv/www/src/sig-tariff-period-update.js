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

class periodUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updatePeriodModal" modal>
				<app-toolbar>
					<h2>Update Period</h2>
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
							value="{{periodRowPrefix}}"
							disabled>
					</paper-input>
					<paper-tooltip
							for="updatePrefix"
							offset="0">
						Prefix to match
					</paper-tooltip>
				<div>
				</div>
					<paper-input
							id="updateDescription"
							name="Description"
							label="Description"
							value="{{periodRowDescription}}">
					</paper-input>
					<paper-tooltip
							for="updateDescription"
							offset="0">
						Description of the prefix
					</paper-tooltip>
				<div>
				</div>
					<paper-input
							id="updateInitialPeriod"
							name="rateInitalUpdate"
							allowed-pattern="[0-9]"
							pattern="^[0-9]+$"
							auto-validate
							label="Initial Period"
							value="{{periodRowInitialPeriod}}">
					</paper-input>
					<paper-tooltip
							for="updateInitialPeriod"
							offset="0">
						Length of the initial period
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateInitialRate"
							name="rateInitalUpdate"
							allowed-pattern="[0-9.]"
							pattern="^[0-9]+\.?[0-9]{0,6}$"
							auto-validate
							label="Rate for Initial Initial Period"
							value="{{periodRowInitialRate}}">
					</paper-input>
					<paper-tooltip
							for="updateInitialRate"
							offset="0">
						Rate for the initial period
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateAddditionalPeriod"
							name="rateInitalUpdate"
							allowed-pattern="[0-9]"
							pattern="^[0-9]+$"
							auto-validate
							label="Addditional Period"
							value="{{periodRowAddditionalPeriod}}">
					</paper-input>
					<paper-tooltip
							for="updateAddditionalPeriod"
							offset="0">
						Length of the additional period
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateAdditionalRate"
							name="rateAdditionalUpdate"
							allowed-pattern="[0-9.]"
							pattern="^[0-9]+\.?[0-9]{0,6}$"
							auto-validate
							label="Rate for Additional Period"
							value="{{periodRowAdditionalRate}}">
					</paper-input>
					<paper-tooltip
							for="updateAdditionalRate"
							offset="0">
						Rate for the additional period
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="update-button"
							on-tap="_updatePeriodRow">
						Update
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							dialog-dismiss
							on-tap="_cancelPeriodRow">
						Cancel
					</paper-button>
					<paper-button toggles
							raised
							on-tap="_deletePeriodRow"
							class="delete-button">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax id="updatePeriodRowAjax"
					on-response="_updatePeriodRowResponse"
					on-error="_updatePeriodRowError"
					on-loading-changed="_onLoadingChanged">
			</iron-ajax>
			<iron-ajax id="deletePeriodRowAjax"
				on-response="_deletePeriodRowResponse"
				on-error="_deletePeriodRowError">
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
			periodRowId: {
				type: String
			},
			periodRowPrefix: {
				type: String
			},
			periodRowDescription: {
				type: String
			},
			periodRowPeriodInitial: {
				type: String
			},
			periodRowRateInitial: {
				type: String
			},
			periodRowPeriodAdditional: {
				type: String
			},
			periodRowRateAdditional: {
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
			this.periodRowId = current.id;
			this.periodRowPrefix = current.prefix;
			this.periodRowDescription = current.description;
			this.periodRowPeriodInitial = current.periodInitial;
			this.periodRowRateInitial = current.rateInitial;
			this.periodRowPeriodAdditional = current.periodAdditional;
			this.periodRowRateAdditional = current.rateAdditional;
			this.$.updatePeriodModal.open();
		}
	}

	_updatePeriodRow(event) {
		var ajax = this.$.updatePeriodRowAjax;
		ajax.method = "PATCH";
		ajax.contentType = "application/json-patch+json";
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.periodRowId;
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
			description.value = {name: "description", value: this.periodRowDescription};
			patch.push(description);
		} else {
			var description = new Object();
			description.op = "replace";
			description.path = "/resourceCharacteristic/" + index + "/value";
			description.value = this.periodRowDescription;
			patch.push(description);
		}
		function isInitialPeriod(element) {
			if(element.name == "initialPeriod") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isInitialPeriod);
		if(index === -1) {
			var initialPeriod = new Object();
			initialPeriod.op = "add";
			initialPeriod.path = "/resourceCharacteristic/-";
			initialPeriod.value = {name: "rate", value: this.periodRowInitialPeriod};
			patch.push(initialPeriod);
		} else {
			var initialPeriod = new Object();
			initialPeriod.op = "replace";
			initialPeriod.path = "/resourceCharacteristic/" + index + "/value";
			initialPeriod.value = this.periodRowInitialPeriod;
			patch.push(initialPeriod);
		}
		function isInitialRate(element) {
			if(element.name == "initialRate") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isInitialRate);
		if(index === -1) {
			var initialRate = new Object();
			initialRate.op = "add";
			initialRate.path = "/resourceCharacteristic/-";
			initialR.value = {name: "rate", value: this.periodRowInitialRate};
			patch.push(initialRate);
		} else {
			var initialRate = new Object();
			initialRate.op = "replace";
			initialRate.path = "/resourceCharacteristic/" + index + "/value";
			initialRate.value = this.periodRowInitialRate;
			patch.push(initialRate);
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isAdditionalPeriod);
		if(index === -1) {
			var additionalPeriod = new Object();
			additionalPeriod.op = "add";
			additionalPeriod.path = "/resourceCharacteristic/-";
			additionalR.value = {name: "rate", value: this.periodRowAdditionalPeriod};
			patch.push(additionalPeriod);
		} else {
			var additionalPeriod = new Object();
			additionalPeriod.op = "replace";
			additionalPeriod.path = "/resourceCharacteristic/" + index + "/value";
			additionalPeriod.value = this.periodRowAdditionalPeriod;
			patch.push(additionalPeriod);
		}
		function isAdditionalRate(element) {
			if(element.name == "additionalRate") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isAdditionalRate);
		if(index === -1) {
			var additionalRate = new Object();
			additionalRate.op = "add";
			additionalRate.path = "/resourceCharacteristic/-";
			additionalR.value = {name: "rate", value: this.periodRowAdditionalRate};
			patch.push(additionalRate);
		} else {
			var additionalRate = new Object();
			additionalRate.op = "replace";
			additionalRate.path = "/resourceCharacteristic/" + index + "/value";
			additionalRate.value = this.periodRowAdditionalRate;
			patch.push(additionalRate);
		}
		ajax.body = JSON.stringify(patch);
		ajax.generateRequest();
		this.$.periodRowId = null;
		this.$.periodRowPrefix = null;
		this.$.periodRowDescription = null;
		this.$.periodRowInitialPeriod = null;
		this.$.periodRowInitialRate = null;
		this.$.periodRowAdditionalPeriod = null;
		this.$.periodRowAdditionalRate = null;
	}

	_updatePeriodRowResponse(event) {
		this.$.updatePeriodModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
	}

	_updatePeriodRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_deletePeriodRow(event) {
		var ajax = this.$.deletePeriodRowAjax;
		ajax.method = "DELETE";
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.$.periodRowPrefix;
		ajax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_deletePeriodRowResponse(event) {
		this.$.updatePeriodModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
	}

	_deletePeriodRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancelPeriodRow() {
		this.$.periodRowId = null;
		this.$.periodRowPrefix = null;
		this.$.periodRowDescription = null;
		this.$.periodRowRate = null;
	}

	_onLoadingChanged(event) {
		if (this.$.updatePeriodRowAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-period-update', periodUpdate);
