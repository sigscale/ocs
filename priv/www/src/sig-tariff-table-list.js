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

class tableList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="tableList">
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
					id="getPrefixRows">
			</iron-ajax>
			<iron-ajax
					id="deletePrefixTable"
					method="DELETE"
					on-response="_deleteTableResponse"
					on-error="_deleteTableError">
			</iron-ajax>
			<iron-ajax
					id="getPrefixTables"
					url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=1,5,7"
					on-response="_getTablesResponse"
					on-error="_getTablesError">
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
				notify: true
			},
			activeTableId: {
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
		var ajax = this.shadowRoot.getElementById('getPrefixTables');
		ajax.generateRequest();
		this.$.tableList.open();
	}

	_getTablesResponse(event) {
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
			this.activeTableName = e.model.item.name;
			this.activeTableId = e.model.item.id;
			this.activeSpecId = e.model.item.specId;
			this.$.tabOkButton.disabled = false;
		} else {
			this.activeTableName = null;
			this.activeTableId = null;
			this.$.tabOkButton.disabled = true;
		}
	}

	_tableOk() {
		document.body.querySelector('sig-app').viewTitle = 'Tariff: ' + this.activeTableName;
		if(this.activeSpecId == "5") {
			var grid = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-periods-list').shadowRoot.getElementById('periodGrid');
			document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-periods-list').shadowRoot.getElementById('getPeriodRows').generateRequest();
		}
		if(this.activeSpecId == "7") {
			var grid = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-roaming-list').shadowRoot.getElementById('roamingGrid');
			document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-roaming-list').shadowRoot.getElementById('getRoamingRows').generateRequest();
		}
		if(this.activeSpecId == "1") {
			var grid = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-rate-list').shadowRoot.getElementById('prefixGrid');
			document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-rate-list').shadowRoot.getElementById('getPrefixRows').generateRequest();
		}
		grid.clearCache();
		this.$.tableList.close();
	} 

	_tableDelete() {
		this.$.deletePrefixTable.url = "/resourceInventoryManagement/v1/resource/" + this.activeTableId;
		this.$.deletePrefixTable.generateRequest();
		this.activeTableName = null;
		this.activeTableId = null;
	}

	_deleteTableResponse(event) {
		this.shadowRoot.getElementById('getPrefixTables').generateRequest();
	}

	_getTablesError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_deleteTableError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-table-add').shadowRoot.getElementById('addPrefixTableModal').open();
	}
}

window.customElements.define('sig-tariff-table-list', tableList);

