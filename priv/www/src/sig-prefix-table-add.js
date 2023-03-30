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

class tablePrefixAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog id="addPrefixTableModal" modal>
				<app-toolbar>
					<h2>Add Table</h2>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<div>
					<paper-input
							label="Name"
							value="{{tableName}}">
					</paper-input>
					<paper-tooltip>
						Tariff table name.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Description"
							value="{{description}}">
					</paper-input>
					<paper-tooltip>
						Tariff table description.
					</paper-tooltip>
				</div>
            <div>
               <paper-dropdown-menu
                     value="{{prefixTableRate}}"
                     no-animations="true"
                     label="Table Type">
                  <paper-listbox
                        id="addTableType"
                        slot="dropdown-content">
                     <paper-item>
                        Prefix Rates
                     </paper-item>
                     <paper-item>
                        Prefix Period Rates
                     </paper-item>
                  </paper-listbox>
               </paper-dropdown-menu>
            </div>
				<div>
					<paper-input
							value="{{startDateTime}}"
							label="Start Date">
					</paper-input>
					<paper-tooltip>
						Start of tariff table validity period.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							value="{{endDateTime}}"
							label="End Date">
					</paper-input>
					<paper-tooltip>
						End of tariff table validity period.
					</paper-tooltip>
				</div>
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
			tableName: {
				type: String
			},
			description: {
				type: String
			},
			prefixTableRate: {
				type: String
			},
			startDateTime: {
				type: String
			},
			endDateTime: {
				type: String
			}
		}
	}

	_tableAdd(event) {
		var tabName = new Object();
		if(this.tableName) {
			tabName.name = this.tableName;
		}
		if(this.description) {
			tabName.description = this.description;
		}
		if(this.startDateTime) {
			var startDateTime = this.startDateTime;
		}
		if(this.endDateTime) {
			var endDateTime = this.endDateTime;
		}
		var taSpec = new Object();
		taSpec.id = "1";
		taSpec.href = "/resourceCatalogManagement/v2/resourceSpecification/1";
		taSpec.name = this.prefixTableRate;
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
		this.tableName = null;
		this.description = null;
		this.prefixTableRate = null;
		this.startDateTime = null;
		this.endDateTime = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_addTableResponse(event) {
		this.$.addPrefixTableModal.close();
		var ajax = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('getPrefixTables');
		ajax.generateRequest();
	}

	_addTableError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_cancel() {
		this.tableName = null;
		this.description = null;
		this.startDateTime = null;
		this.endDateTime = null;
	}
}

window.customElements.define('sig-prefix-table-add', tablePrefixAdd);
