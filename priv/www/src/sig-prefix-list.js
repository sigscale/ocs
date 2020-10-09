/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
 */

import { PolymerElement, html } from '@polymer/polymer/polymer-element.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/iron-icons/iron-icons.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@polymer/paper-toast/paper-toast.js';
import './style-element.js'

class prefixList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="prefixGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column width="15ex">
					<template class="header">
						Prefix
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
			</vaadin-grid>
			<paper-dialog class="dialog" id="tableList">
				<app-toolbar>
					List of Tables
				</app-toolbar>
				<template is="dom-repeat" items="[[tables]]">
					<paper-item
							id="pagePrefix"
							class="menuitem"
							on-focused-changed="tableSelection">
						<iron-icon icon ="icons:view-list" item-icon></iron-icon>
							{{item.id}}
					</paper-item>
				</template>
				<div class="buttons">
					<paper-button
							raised
							id="tabOkButton"
							disabled
							on-tap="tableOk"
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
							on-tap="tableAdd"
							class="submit-button">
						Add
					</paper-button>
					<paper-button
							raised
							on-tap="tableDelete"
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
			<iron-ajax id="getTableContentAjax"
					on-response="_getTableContentResponse"
					on-error="_getTableContentError">
			</iron-ajax>
			<iron-ajax id="deleteTableAjax"
				on-response="_deleteTableResponse"
				on-error="_deleteTableError">
			</iron-ajax>
			<iron-ajax id="getTableAjax"
				on-response="_getTableResponse"
				on-error="_getTableError">
			</iron-ajax>
			<paper-toast id="getPrefixToast">
			</paper-toast>
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
			table: {
				type: String
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('prefixGrid');
		var ajax1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-list').shadowRoot.getElementById('getTableAjax');
		ajax1.url = "/catalogManagement/v2/pla";
		ajax1.generateRequest();
		this.$.tableList.open();
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.prefixGrid.selectedItems = item ? [item] : [];
		} else {
			this.$.prefixGrid.selectedItems = [];
		}
	}

	_getTableResponse(event) {
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			tableRecord.description = results[indexTable].description;
			tableRecord.plaSpecId = results[indexTable].plaSpecId;
			this.push('tables', tableRecord);
		}
	}

	tableOk() {
		var grid = this.shadowRoot.getElementById('prefixGrid');
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid');
		grid.dataProvider = this._getPreTable;
		this.$.tableList.close();
	}

	tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-table-add').shadowRoot.getElementById('addPrefixTableModal').open();
		this.$.tableList.close();
	}

	tableDelete(event) {
		this.$.deleteTableAjax.method = "DELETE";
		var prefixListDel = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-list');
		this.$.deleteTableAjax.url = "/catalogManagement/v2/pla/" + prefixListDel.table;
		this.$.deleteTableAjax.generateRequest();
	}

	_deleteTableResponse(event) {
		this.$.tableList.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('offerList').shadowRoot.getElementById('getTableAjax').generateRequest();
		this.$.getPrefixToast.text = "Deleted successfully";
		this.$.getPrefixToast.open();
	}

	_deleteTableError(event) {
		this.$.getPrefixToast.text = "Error";
		this.$.getPrefixToast.open();
	}

	tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').table = e.model.item.id;
			this.$.tabOkButton.disabled = false;
		} else {
			this.$.tabOkButton.disabled = true;
		}
	}

	_getPreTable(params, callback) {
		var grid = this;
		var prefixList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-list');
		var ajax = prefixList.shadowRoot.getElementById('getTableContentAjax');
		ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + prefixList.table;
		var handleAjaxResponse = function(request) {
			if(request) {
				grid.size = 100;
				var vaadinItems = new Array();
				for (var index in request.response) {
					var resChar = request.response[index].resourceCharacteristic;
					var tabObj = new Object();
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "prefix") {
							tabObj.prefix = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "description") {
							tabObj.description = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "rate") {
							tabObj.rate = resChar[indexRes].value.value;
						}
						vaadinItems[index] = tabObj;
					}
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			prefixList.etag = null;
			this.$.getPrefixToast.text = "Error";
			this.$.getPrefixToast.open();
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

	showAddPrefixModal() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-prefix-add').shadowRoot.getElementById('addPrefixModal').open();
	}
}

window.customElements.define('sig-prefix-list', prefixList);

