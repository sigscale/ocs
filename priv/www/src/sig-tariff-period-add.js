/**
 * Copyright 2016 - 2024 SigScale Global Inc.
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

class periodAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addPeriodModal" modal>
				<app-toolbar>
					<h2>Add Period</h2>
				</app-toolbar>
				<paper-progress
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
					<div>
						<paper-input
								allowed-pattern="[+0-9]"
								pattern="^[+]?[0-9]+"
								auto-validate
								label="Prefix"
								value="{{periodRowPrefix}}">
						</paper-input>
						<paper-tooltip>
							Leading digits to match against an origination, destination or VPLMN address.
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								label="Description"
								value="{{periodRowDescription}}">
						</paper-input>
						<paper-tooltip>
							Description of addresses matching this prefix.
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								allowed-pattern="[0-9.]"
								pattern="^[0-9]+$"
								auto-validate
								label="Initial Period Duration"
								value="{{periodRowPeriodInitial}}">
						</paper-input>
						<paper-tooltip>
							Length of initial period in seconds
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								allowed-pattern="[0-9.]"
								pattern="^[0-9]+\.?[0-9]{0,6}$"
								auto-validate
								label="Initial Period Rate"
								value="{{periodRowRateInitial}}">
						</paper-input>
						<paper-tooltip>
							Rated price for the initial period
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								allowed-pattern="[0-9.]"
								pattern="^[0-9]+$"
								auto-validate
								label="Additional Period Duration"
								value="{{periodRowPeriodAdditional}}">
						</paper-input>
						<paper-tooltip>
							Length of additional period in seconds
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								allowed-pattern="[0-9.]"
								pattern="^[0-9]+\.?[0-9]{0,6}$"
								auto-validate
								label="Additional Period Rate"
								value="{{periodRowRateAdditional}}">
						</paper-input>
						<paper-tooltip>
							Rated price for each additional period
						</paper-tooltip>
					</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							on-tap="_periodTableRow"
							class="submit-button">
						Submit
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancelPeriodRow"
							dialog-dismiss>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="addPeriodTableRow"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addPeriodRowResponse"
					on-error="_addPeriodRowError">
			</iron-ajax>
		`;
	}
	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
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

	_periodTableRow(event) {
		var periodList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-periods-list')
		var ajax = this.$.addPeriodTableRow;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource";
		var row = new Object();
		var rel = new Array();
		var relObj = new Object();
		relObj.id = periodList.activeTableId;
		relObj.href = "/resourceInventoryManagement/v1/resourceRelationship/" + relObj.id;
		relObj.name = periodList.activeTableName;
		var relObj1 = new Object();
		relObj1.relationshipType = "contained";
		relObj1.resource = relObj
		rel.push(relObj1);
		row.resourceRelationship = rel

		var resource = new Array();
		var resPre = new Object();
		resPre.name = "prefix";
		resPre.value = this.periodRowPrefix;
		resource.push(resPre);
		var resDes = new Object();
		resDes.name = "description";
		resDes.value = this.periodRowDescription;
		resource.push(resDes);
		var resIniDuration = new Object();
		resIniDuration.name = "periodInitial";
		resIniDuration.value = parseInt(this.periodRowPeriodInitial);
		resource.push(resIniDuration);
		var resIniRate = new Object();
		resIniRate.name = "rateInitial";
		resIniRate.value = this.periodRowRateInitial;
		resource.push(resIniRate);
		var resAddDuration = new Object();
		resAddDuration.name = "periodAdditional";
		resAddDuration.value = parseInt(this.periodRowPeriodAdditional);
		resource.push(resAddDuration);
		var resAddRate = new Object();
		resAddRate.name = "rateAdditional";
		resAddRate.value = this.periodRowRateAdditional;
		resource.push(resAddRate);
		row.resourceCharacteristic = resource;

		var spec = new Object();
		spec.id = "6";
		spec.name = "TariffPeriodsTableRow";
		spec.href = "resourceCatalogManagement/v2/resourceSpecification/" + "6";
		row.resourceSpecification = spec;
		ajax.body = row;
		ajax.generateRequest();
	}

	_addPeriodRowResponse(event) {
		this.$.addPeriodModal.close();
		this.periodRowPrefix = null;
		this.periodRowDescription = null;
		this.periodRowPeriodInitial = null;
		this.periodRowRateInitial = null;
		this.periodRowPeriodAdditional = null;
		this.periodRowRateAdditional = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_addPeriodRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancelPeriodRow() {
		this.periodRowPrefix = null;
		this.periodRowDescription = null;
		this.periodRowPeriodInitial = null;
		this.periodRowRateInitial = null;
		this.periodRowPeriodAdditional = null;
		this.periodRowRateAdditional = null;
	}

	_onLoadingChanged(event) {
		if (this.$.addPeriodTableRow.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-period-add', periodAdd);
