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
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import './style-element.js'

class tariffPeriodList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="tariffPeriodList">
				<app-toolbar>
					List of Tables
				</app-toolbar>
				<template
						id="tableItems"
						is="dom-repeat"
						items="[[tables]]">
					<paper-item
							id="pagePrefix"
							class="menuitem"
							on-focused-changed="_tableSelection">
						<iron-icon icon="icons:view-list" item-icon></iron-icon>
							{{item.name}}
					</paper-item>
				</template>
				<div class="buttons">
					<paper-button
							raised
							id="tabOkButton"
							disabled
							on-tap="_tableOk"
							class="submit-button">
						Ok
					</paper-button>
					<paper-button
							dialog-dismiss
							class="cancel-button">
						Cancel
					</paper-button>
					<paper-button
							raised
							on-tap="_tableAdd"
							class="submit-button">
						Add
					</paper-button>
					<paper-button
							raised
							on-tap="_tableDelete"
							class="delete-button">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="deletePeriodTable"
					method="DELETE"
					on-response="_deletePeriodResponse"
					on-error="_deletePeriodError">
			</iron-ajax>
			<iron-ajax
					id="getPeriodTables"
					url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=5"
					on-response="_getPeriodResponse"
					on-error="_getPeriodError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			etag: {
				type: String,
				value: null
			},
			tables: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			activeTableName: {
				type: String,
				notify: true,
				readOnly: true
			},
			activeTable: {
				type: String
			},
			activeItem: {
				type: Object,
				notify: true
			},
			activeSpecId: {
				type: String,
			}
		}
	}

	ready() {
		super.ready();
	}

	_getPeriodResponse(event) {
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.name = results[indexTable].name;
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			tableRecord.specId = results[indexTable].resourceSpecification.id;
			this.push('tables', tableRecord);
		}
		this.shadowRoot.getElementById('tableItems').notifyPath('items');
	}

	_tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			this._setActiveTableName(e.model.item.name);
			this.activeTable = e.model.item.id;
			this.activeSpecId = e.model.item.specId;
			this.$.tabOkButton.disabled = false;
		} else {
			this._setActiveTableName(null);
			this.activeTable = null;
			this.$.tabOkButton.disabled = true;
		}
	}

	_tableOk() {
		var sigApp = document.body.querySelector('sig-app');
		document.body.querySelector('sig-app').viewTitle = 'Tariff: ' + this.activeTableName;
		if(this.activeSpecId == "5") {
			sigApp.shadowRoot.querySelector('sig-tariff-periods-list').shadowRoot.getElementById('getPeriodRows').generateRequest();
			sigApp.shadowRoot.getElementById('periodList').shadowRoot.getElementById('periodGrid').clearCache();
		}
		this.$.tariffPeriodList.close();
	} 

	_tableDelete() {
		this.$.deletePeriodTable.url = "/resourceInventoryManagement/v1/resource/" + this.activeTable;
		this.$.deletePeriodTable.generateRequest();
		this.activeTableName = null;
		this.activeTable = null;
	}

	_deletePeriodResponse(event) {
		this.shadowRoot.getElementById('getPeriodTables').generateRequest();
	}

	_getPeriodError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_deletePeriodError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-table-add').shadowRoot.getElementById('addPrefixTableModal').open();
	}
}

window.customElements.define('sig-period-table-list', tariffPeriodList);

