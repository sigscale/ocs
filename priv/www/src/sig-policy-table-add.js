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

class tablePolAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog id="addPolicyTableModal" modal>
				<app-toolbar>
					<h2>Add Table</h2>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="addTableName"
						name="name"
						label="Name"
						value="{{addName}}">
				</paper-input>
				<paper-tooltip
						for="addTableName"
						offset="0">
					Table name
				</paper-tooltip>
				<paper-input
						id="addTableDesc"
						name="description"
						label="Description"
						value="{{addDesc}}">
				</paper-input>
				<paper-tooltip
						for="addTableDesc"
						offset="0">
					Table description
				</paper-tooltip>
				<paper-input
						id="addTableStart"
						value="{{startTableTimePick}}"
						name="startDate"
						label="Start Date">
				</paper-input>
				<paper-tooltip
						for="addTableStart"
						offset="0">
					Table start date time
				</paper-tooltip>
				<paper-input
						id="addTableEnd"
						value="{{endTableTimePick}}"
						name="endDate"
						label="End Date">
				</paper-input>
				<paper-tooltip
						for="addTableEnd"
						offset="0">
					Table end date time
				</paper-tooltip>
				<paper-tooltip
						for="addTableSpec"
						offset="0">
					Add Specification
				</paper-tooltip>
				<div class="buttons">
					<paper-button
							dialog-confirm
							raised
							on-tap="_tableAdd"
							class="submit-button">
						Submit
					</paper-button>
					<paper-button
							dialog-dismiss
							on-tap="_cancel"
							class="cancel-button">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="addTable"
					url="resourceInventoryManagement/v1/resource"
					method = "POST"
					content-type="application/json"
					loading="{{loading}}"
					on-response="_addTableResponse"
					on-error="_addTableError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			addName: {
				type: String
			},
			addDesc: {
				type: String
			},
			startTableTimePick: {
				type: String
			},
			endTableTimePick: {
				type: String
			}
		}
	}

	_tableAdd(event) {
		var tabName = new Object();
		if(this.addName) {
			tabName.name = this.addName;
		}
		if(this.addDesc) {
			tabName.description = this.addDesc;
		}
		if(this.startTableTimePick) {
			var startDateTime = this.startTableTimePick;
		}
		if(this.endTableTimePick) {
			var endDateTime = this.endTableTimePick;
		}
		var taSpec = new Object();
		taSpec.id = "3";
		taSpec.href = "/resourceCatalogManagement/v2/resourceSpecification/3";
		taSpec.name = "PolicyTable";
		tabName.resourceSpecification = taSpec;
		if(endDateTime < startDateTime) {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		} else if(startDateTime && endDateTime) {
			tabName.validFor = {startDateTime, endDateTime};
		} else if(startDateTime && !endDateTime) {
			tabName.validFor = {startDateTime};
		} else if(!startDateTime && !endDateTime) {
			tabName.validFor = {endDateTime};
		}
		if(tabName.name) {
			var ajax = this.$.addTable;
			ajax.body = tabName;
			ajax.generateRequest();
		}
		this.addName = null;
		this.addDesc = null;
		this.startTableTimePick = null;
		this.endTableTimePick = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

	_addTableResponse(event) {
		this.$.addPolicyTableModal.close();
		var ajax = document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('getPolicyTables');
		ajax.generateRequest();
	}

	_addTableError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancel() {
		this.addName = null;
		this.addDesc = null;
		this.startTableTimePick = null;
		this.endTableTimePick = null;
	}
}

window.customElements.define('sig-policy-table-add', tablePolAdd);
