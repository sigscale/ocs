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
			activeTableId: {
				type: String
			},
			activeTableHref: {
				type: String
			},
			activeTableName: {
				type: String
			}
		}
	}

	ready() {
		super.ready();
	}

	_getPeriodResponse(event) {
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length);
		for (var index in results) {
			var table = new Object();
			table.id = results[index].id;
			table.href = results[index].href;
			table.name = results[index].name;
			this.push('tables', table);
		}
		this.shadowRoot.getElementById('tableItems').notifyPath('items');
	}

	_tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			this.activeTableId = e.model.item.id;
			this.activeTableHref = e.model.item.href;
			this.activeTableName = e.model.item.name;
			this.$.tabOkButton.disabled = false;
		} else {
			this.activeTableId = null;
			this.activeTableHref = null;
			this.activeTableName = null;
			this.$.tabOkButton.disabled = true;
		}
	}

	_tableOk() {
		var app = document.body.querySelector('sig-app');
		app.viewTitle = 'Tariff: ' + this.activeTableName;
		var periodList = app.shadowRoot.getElementById('periodList');
		periodList.activeTableId = this.activeTableId;
		periodList.activeTableName = this.activeTableName;
		periodList.shadowRoot.getElementById('periodGrid').clearCache();
		this.$.tariffPeriodList.close();
	}

	_tableDelete() {
		this.$.deletePeriodTable.url = this.activeTableHref;
		this.$.deletePeriodTable.generateRequest();
		this.activeTableId = null;
		this.activeTableHref = null;
		this.activeTableName = null;
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
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-period-table-add').shadowRoot.getElementById('addPeriodTableModal').open();
	}
}

window.customElements.define('sig-period-table-list', tariffPeriodList);

