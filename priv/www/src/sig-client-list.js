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

class clientList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="clientGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="address"
								path="id"
								value="{{_filterAddress}}">
							<input
									slot="filter"
									placeholder="Address"
									value="{{_filterAddress::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.id]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="port"
								path="port"
								value="[[_filterPort]]">
							<input
									slot="filter"
									placeholder="Port"
									value="{{_filterPort::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.port]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="identifier"
								path="identifier"
								value="[[_filterIdentifier]]">
							<input
									slot="filter"
									placeholder="Identifier"
									value="{{_filterIdentifier::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.identifier]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="secret"
								path="secret"
								value="[[_filterSecret]]">
							<input
									slot="filter"
									placeholder="Secret"
									value="{{_filterSecret::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.secret]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="protocol"
								path="protocol"
								value="[[_filterProtocol]]">
							<input
									slot="filter"
									placeholder="Protocol"
									value="{{_filterProtocol::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.protocol]]
					</template>
				</vaadin-grid-column>
            <vaadin-grid-column>
               <template class="header">
                  TWAN
               </template>
               <template>[[item.trusted]]</template>
            </vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap = "showAddClientModal">
				</paper-fab>
			</div>
			<iron-ajax
					id="getClientAjax"
					url="/ocs/v1/client"
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
			etag: {
				type: String,
				value: null
			},
			activeItem: {
				type: Object,
				notify: true
			},
			_filterAddress: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterPort: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterIdentifier: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSecret: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterProtocol: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	refreshClient() {
		this.etag = null;
		delete this.$.getClientAjax.headers['If-Range'];
		delete this.$.getClientAjax.params['filter'];
		this._filterAddress = null;
		this._filterPort = null;
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('clientGrid');
		grid.dataProvider = this._getClient;
	}

	_getClient(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var clientList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-client-list');
		var ajax = clientList.shadowRoot.getElementById('getClientAjax');
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "id" || param.path == "port"
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if(filter.value) {
				if(ajax.params['filter']) {
					ajax.params['filter'] += "]," + filter.path + ".like=[" + filter.value + "%";
				} else {
					ajax.params['filter'] = "\"[{" + filter.path + ".like=[" + filter.value + "%";
				}
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				clientList.etag = request.xhr.getResponseHeader('ETag');
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
					return characteristic.name == "id"
				}
				for (var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					newRecord.identifier = request.response[index].identifier;
					newRecord.secret = request.response[index].secret;
					newRecord.passwordRequired = request.response[index].passwordRequired;
					newRecord.port = request.response[index].port;
					newRecord.protocol = request.response[index].protocol;
					newRecord.trusted = request.response[index].trusted;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			clientList.etag = null;
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
				if (clientList.etag && params.page > 0) {
					ajax.headers['If-Range'] = clientList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (clientList.etag && params.page > 0) {
				ajax.headers['If-Range'] = clientList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('clientGrid');
		grid.size = 0;
	}

	showAddClientModal(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-client-add').shadowRoot.getElementById('clientAddModal').open();
	}
}

window.customElements.define('sig-client-list', clientList);

