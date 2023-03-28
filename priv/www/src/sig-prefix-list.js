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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js'

class prefixList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="prefixGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				<vaadin-grid-column width="15ex">
					<template class="header">
						<vaadin-grid-filter
								aria-label="prefix"
								path="prefix"
								value="{{_filterPrefix}}">
							<input
									slot="filter"
									placeholder="Prefix"
									value="{{_filterPrefix::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.prefix]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="15ex">
					<template class="header">
							Description
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="15ex">
					<template class="header">
							Rate
					</template>
					<template>[[item.rate]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
               <template class="header">
                  <div class="grouptitle">Initial Period</div>
               </template>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Seconds
						</template>
						<template>
							<div class="cell numeric">
								[[item.seconds]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Rate
						</template>
						<template>
							<div class="cell numeric">
								[[item.rate]]
							</div>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column-group>
               <template class="header">
                  <div class="grouptitle">Additional Period</div>
               </template>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Seconds
						</template>
						<template>
							<div class="cell numeric">
								[[item.seconds]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Rate
						</template>
						<template>
							<div class="cell numeric">
								[[item.rate]]
							</div>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
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
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="showAddPrefixModal">
				</paper-fab>
			</div>
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
					url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=1"
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
			_filterPrefix: {
				type: Boolean,
				observer: '_filterChanged'
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
			this.push('tables', tableRecord);
		}
		this.shadowRoot.getElementById('tableItems').notifyPath('items');
	}

	_tableOk() {
		document.body.querySelector('sig-app').viewTitle = 'Tariff: ' + this.activeTableName;
		var grid = this.shadowRoot.getElementById('prefixGrid');
		grid.dataProvider = this._getPrefixTable;
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

	_tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			this.activeTableName = e.model.item.name;
			this.activeTableId = e.model.item.id;
			this.$.tabOkButton.disabled = false;
		} else {
			this.activeTableName = null;
			this.activeTableId = null;
			this.$.tabOkButton.disabled = true;
		}
	}

	_getPrefixTable(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var prefixList = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList');
		var ajax = prefixList.shadowRoot.getElementById('getPrefixRows');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=2&resourceRelationship.resource.name=" + prefixList.activeTableName;
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "prefix"
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if(filter.value) {
				if(ajax.params['filter']) {
					ajax.params['filter'] += "]," + "resourceCharacteristic." + filter.path + ".like=[" + filter.value + "%";
				} else {
					ajax.params['filter'] = "\"[{" + "resourceCharacteristic." +filter.path + ".like=[" + filter.value + "%";
				}
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				prefixList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic) {
					return characteristic.name == "name"
				}
				for (var index in request.response) {
					var tabObj = new Object();
					tabObj.id = request.response[index].id;
					tabObj.resourceCharacteristic = request.response[index].resourceCharacteristic;
					var resChar = request.response[index].resourceCharacteristic;
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "prefix") {
							tabObj.prefix = resChar[indexRes].value;
						}
						if(resChar[indexRes].name == "description") {
							tabObj.description = resChar[indexRes].value;
						}
						if(resChar[indexRes].name == "rate") {
							tabObj.rate = resChar[indexRes].value;
						}
						vaadinItems[index] = tabObj;
					}
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			prefixList.etag = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (prefixList.etag && params.page > 0) {
					ajax.headers['If-Range'] = prefixList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (prefixList.etag && params.page > 0) {
				ajax.headers['If-Range'] = prefixList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('prefixGrid');
		grid.size = 0;
	}

	_tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-table-add').shadowRoot.getElementById('addPrefixTableModal').open();
	}

	showAddPrefixModal() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-add').shadowRoot.getElementById('addPrefixModal').open();
	}
}

window.customElements.define('sig-prefix-list', prefixList);

