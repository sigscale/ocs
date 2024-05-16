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

class rateAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addPrefixModal" modal>
				<app-toolbar>
					<h2>Add Prefix</h2>
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
								value="{{rateRowPrefix}}">
						</paper-input>
						<paper-tooltip>
							Leading digits to match against an origination, destination or VPLMN address.
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								label="Description"
								value="{{rateRowDescription}}">
						</paper-input>
						<paper-tooltip>
							Description of addresses matching this prefix.
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								allowed-pattern="[0-9.]"
								pattern="^[0-9]+\.?[0-9]{0,6}$"
								auto-validate
								label="Rate"
								value="{{rateRowRate}}">
						</paper-input>
						<paper-tooltip>
							Rate tariff table to apply for rating in the selected VPLMN
						</paper-tooltip>
					</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							on-tap="_tableRow"
							class="submit-button">
						Submit
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancelRateRow"
							dialog-dismiss>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="addTableRow"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addRateRowResponse"
					on-error="_addRateRowError">
			</iron-ajax>
		`;
	}
	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			rateRowPrefix: {
				type: String
			},
			rateRowDescription: {
				type: String
			},
			rateRowRate: {
				type: Number
			}
		}
	}

	ready() {
		super.ready()
	}

	_tableRow(event) {
		var rateList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-rate-list')
		var ajax = this.$.addTableRow;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource/";
		var row = new Object();
		var rel = new Array();
		var relObj = new Object();
		relObj.id = rateList.activeTableId;
		relObj.href = "/resourceInventoryManagement/v1/resourceRelationship/" + relObj.id;
		relObj.name = rateList.activeTableName;
		var relObj1 = new Object();
		relObj1.relationshipType = "contained";
		relObj1.resource = relObj
		rel.push(relObj1);
		row.resourceRelationship = rel

		var resource = new Array();
		var resPre = new Object();
		resPre.name = "prefix";
		resPre.value = this.rateRowPrefix;
		resource.push(resPre);
		var resDes = new Object();
		resDes.name = "description";
		resDes.value = this.rateRowDescription;
		resource.push(resDes);
		var resRate = new Object();
		resRate.name = "rate";
		resRate.value = this.rateRowRate;
		resource.push(resRate);
		row.resourceCharacteristic = resource;

		var spec = new Object();
		spec.id = "2";
		spec.name = "TariffTableRow";
		spec.href = "resourceCatalogManagement/v2/resourceSpecification/" + "2";
		row.resourceSpecification = spec;
		ajax.body = row;
		ajax.generateRequest();
	}

	_addRateRowResponse(event) {
		this.$.addPrefixModal.close();
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('rateList').shadowRoot.getElementById('rateGrid').clearCache();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_addRateRowError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancelRateRow() {
		this.rateRowPrefix = null;
		this.rateRowDescription = null;
		this.rateRowRate = null;
	}

	_onLoadingChanged(event) {
		if (this.$.addTableRow.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-rate-add', rateAdd);
