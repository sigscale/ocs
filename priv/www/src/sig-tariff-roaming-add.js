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

class roamingAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addRoamingModal" modal>
				<app-toolbar>
					<h2>Add Roaming</h2>
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
								value="{{addPre}}">
						</paper-input>
						<paper-tooltip>
							Leading digits to match against an origination, destination or VPLMN address.
						</paper-tooltip>
					</div>
					<div>
						<paper-input
								label="Description"
								value="{{addPreDesc}}">
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
								label="Tariff Table Name"
								value="{{addPreTariffTableName}}">
						</paper-input>
						<paper-tooltip>
							Price per unit to apply when this prefix matches an address.
						</paper-tooltip>
					</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							on-tap="_roamingRow"
							class="submit-button">
						Submit
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel"
							dialog-dismiss>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="addRoamingRow"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addRoamingResponse"
					on-error="_addRoamingError">
			</iron-ajax>
		`;
	}
	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			addPre: {
				type: String,
			},
			addPreDesc: {
				type: String,
			},
			addPreTariffTableName: {
				type: String,
			}
		}
	}

	ready() {
		super.ready()
	}

	_roamingRow(event) {
		var roamingTable = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-roaming-table-list')
		var ajax = this.$.addRoamingRow;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource/";
		var tar = new Object();
		var rel = new Array();
		var relObj = new Object();
		relObj.id = roamingTable.activeTable;
		relObj.href = "/resourceInventoryManagement/v1/resourceRelationship/" + relObj.id;
		relObj.name = roamingTable.activeTableName;
		var relObj1 = new Object();
		relObj1.relationshipType = "contained";
		relObj1.resource = relObj
		rel.push(relObj1);
		tar.resourceRelationship = rel

		var resource = new Array();
		var resPre = new Object();
		resPre.name = "prefix";
		resPre.value = this.addPre;
		resource.push(resPre);
		var resDes = new Object();
		resDes.name = "description";
		resDes.value = this.addPreDesc;
		resource.push(resDes);
		var resRate = new Object();
		resRate.name = "rate";
		resRate.value = this.addPreTariffTableName;
		resource.push(resRate);
		tar.resourceCharacteristic = resource;

		var spec = new Object();
		spec.id = "8";
		spec.name = "RoamingTableRow";
		spec.href = "resourceCatalogManagement/v2/resourceSpecification/" + "8";
		tar.resourceSpecification = spec;
		ajax.body = tar;
		ajax.generateRequest();
		this.$.addRoamingModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid').clearCache();
	}

	_addRoamingResponse(event) {
		this.$.addRoamingModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList').shadowRoot.getElementById('roamingGrid').clearCache();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
	}

	_addRoamingError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancel() {
		this.addPre = null;
		this.addDesc = null;
		this.addRateRow = null;
	}

	_onLoadingChanged(event) {
		if (this.$.addRoamingRow.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-tariff-roaming-add', roamingAdd);
