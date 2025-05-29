/**
 * Copyright 2016 - 2025 SigScale Global Inc.
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
							value="{{periodRowPeriodInitial}}">
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
							value="{{periodRowRateInitial}}">
					</paper-input>
					<paper-tooltip
							for="updateInitialRate"
							offset="0">
						Rate for the initial period
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="updateAdeitionalPeriod"
							name="rateInitalUpdate"
							allowed-pattern="[0-9]"
							pattern="^[0-9]+$"
							auto-validate
							label="Additional Period"
							value="{{periodRowPeriodAdditional}}">
					</paper-input>
					<paper-tooltip
							for="updateAdditionalPeriod"
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
							value="{{periodRowRateAdditional}}">
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
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updatePeriodRowResponse"
					on-error="_updatePeriodRowError"
					on-loading-changed="_onLoadingChanged">
			</iron-ajax>
			<iron-ajax id="deletePeriodRowAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
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
				type: Number
			},
			periodRowRateInitial: {
				type: Number
			},
			periodRowPeriodAdditional: {
				type: Number
			},
			periodRowRateAdditional: {
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
		} else if(this.periodRowDescription != this.activeItem.resourceCharacteristic[index].value) {
			var description = new Object();
			description.op = "replace";
			description.path = "/resourceCharacteristic/" + index + "/value";
			description.value = this.periodRowDescription;
			patch.push(description);
		}
		function isPeriodInitial(element) {
			if(element.name == "periodInitial") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isPeriodInitial);
		if(index === -1) {
			var initialPeriod = new Object();
			initialPeriod.op = "add";
			initialPeriod.path = "/resourceCharacteristic/-";
			initialPeriod.value = {name: "periodInitial", value: this.periodRowPeriodInitial};
			patch.push(initialPeriod);
		} else if(this.periodRowPeriodInitial != this.activeItem.resourceCharacteristic[index].value) {
			var initialPeriod = new Object();
			initialPeriod.op = "replace";
			initialPeriod.path = "/resourceCharacteristic/" + index + "/value";
			initialPeriod.value = this.periodRowPeriodInitial;
			patch.push(initialPeriod);
		}
		function isRateInitial(element) {
			if(element.name == "rateInitial") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isRateInitial);
		if(index === -1) {
			var initialRate = new Object();
			initialRate.op = "add";
			initialRate.path = "/resourceCharacteristic/-";
			initialRate.value = {name: "rateInitial", value: this.periodRowRateInitial};
			patch.push(initialRate);
		} else if(this.periodRowRateInitial != this.activeItem.resourceCharacteristic[index].value) {
			var initialRate = new Object();
			initialRate.op = "replace";
			initialRate.path = "/resourceCharacteristic/" + index + "/value";
			initialRate.value = this.periodRowRateInitial;
			patch.push(initialRate);
		}
		function isPeriodAdditional(element) {
			if(element.name == "periodAdditional") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isPeriodAdditional);
		if(index === -1) {
			var additionalPeriod = new Object();
			additionalPeriod.op = "add";
			additionalPeriod.path = "/resourceCharacteristic/-";
			additionalPeriod.value = {name: "periodAdditional", value: this.periodRowPeriodAdditional};
			patch.push(additionalPeriod);
		} else if(this.periodRowPeriodAdditional != this.activeItem.resourceCharacteristic[index].value) {
			var additionalPeriod = new Object();
			additionalPeriod.op = "replace";
			additionalPeriod.path = "/resourceCharacteristic/" + index + "/value";
			additionalPeriod.value = this.periodRowPeriodAdditional;
			patch.push(additionalPeriod);
		}
		function isRateAdditional(element) {
			if(element.name == "rateAdditional") {
				return true;
			} else {
				return false;
			}
		}
		var index = this.activeItem.resourceCharacteristic.findIndex(isRateAdditional);
		if(index === -1) {
			var additionalRate = new Object();
			additionalRate.op = "add";
			additionalRate.path = "/resourceCharacteristic/-";
			additionalRate.value = {name: "rateAdditional", value: this.periodRowRateAdditional};
			patch.push(additionalRate);
		} else if(this.periodRowRateAdditional != this.activeItem.resourceCharacteristic[index].value) {
			var additionalRate = new Object();
			additionalRate.op = "replace";
			additionalRate.path = "/resourceCharacteristic/" + index + "/value";
			additionalRate.value = this.periodRowRateAdditional;
			patch.push(additionalRate);
		}
		ajax.body = JSON.stringify(patch);
		ajax.generateRequest();
	}

	_updatePeriodRowResponse(event) {
		this.$.updatePeriodModal.close();
		this.periodRowId = null;
		this.periodRowPrefix = null;
		this.periodRowDescription = null;
		this.periodRowPeriodInitial = null;
		this.periodRowRateInitial = null;
		this.periodRowPeriodAdditional = null;
		this.periodRowRateAdditional = null;
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
		ajax.url = "/resourceInventoryManagement/v1/resource/" + this.periodRowId;
		ajax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
	}

	_deletePeriodRowResponse(event) {
		this.$.updatePeriodModal.close();
		this.periodRowId = null;
		this.periodRowPrefix = null;
		this.periodRowDescription = null;
		this.periodRowPeriodInitial = null;
		this.periodRowRateInitial = null;
		this.periodRowPeriodAdditional = null;
		this.periodRowRateAdditional = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
	}

	_deletePeriodRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancelPeriodRow() {
		this.periodRowId = null;
		this.periodRowPrefix = null;
		this.periodRowDescription = null;
		this.periodRowPeriodInitial = null;
		this.periodRowRateInitial = null;
		this.periodRowPeriodAdditional = null;
		this.periodRowRateAdditional = null;
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
