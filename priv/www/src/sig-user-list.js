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
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js';

class userList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="userGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter slot="filter" id="filterId" aria-label="User Name" path="id" value="{{_filterId}}">
							<input placeholder="Username" value="{{_filterId::input}}" focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter slot="filter" id="filterLanguage" aria-label="Language" path="language" value="{{_filterLanguage}}">
							<input placeholder="Language" value="{{_filterLanguage::input}}" focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.language]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddUserModal">
				</paper-fab>
			</div>
			<iron-ajax
				id="getUserAjax"
				url="/partyManagement/v1/individual"
				rejectWithRequest>
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				notify: true
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			},
			etag: {
				type: String,
				value: null
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('userGrid');
		grid.dataProvider = this._getUsers;
	}

_activeItemChanged(item) {
		if(item) {
			this.$.userGrid.selectedItems = item ? [item] : [];
		} else {
			this.$.userGrid.selectedItems = [];
		}
	}

	_getUsers(params, callback) {
		var grid = this;
		if(!grid.size) {
				grid.size = 0;
		}
		var userList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-user-list');
		var ajax = userList.shadowRoot.getElementById('getUserAjax');
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "id";
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if (filter.value) {
				ajax.params['filter'] = "\"[{id.like=[" + filter.value + "%";
			}
		});
		function checkChar(param) {
			return param.path != "id";
		}
		params.filters.filter(checkChar).forEach(function(filter) {
			if (filter.value) {
				if (!ajax.params['filter']) {
					ajax.params['filter'] = "\"[{";
				} else {
					ajax.params['filter'] += "],";
				}
				ajax.params['filter'] += "characteristic.contains=[{name=language" + ",value.like=[" + filter.value + "%]}";
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		if(userList.etag && params.page > 0) {
			ajax.headers['If-Range'] = userList.etag;
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				userList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic){
					return characteristic.name == "locale";
				}
				for(var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					var langChar = request.response[index].characteristic.find(checkChar);
					if(langChar != undefined) {
						newRecord.language = langChar.value;
					}
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			userList.etag = null;
			var toast = document.body.querySelector('inventory-management').shadowRoot.getElementById('restError');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (userList.etag && params.page > 0) {
					ajax.headers['If-Range'] = userList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (userList.etag && params.page > 0) {
				ajax.headers['If-Range'] = userList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	showAddUserModal(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-user-add').shadowRoot.getElementById('addUserModal').open();
	}
}

window.customElements.define('sig-user-list', userList);
