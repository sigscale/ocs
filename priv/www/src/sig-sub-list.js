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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js'

class subList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="subscriberGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
							id="filter"
							aria-label="Identity"
							path="id"
							value="{{_filterIdentity}}">
							<input
									slot="filter"
									placeholder="Identity"
									value="{{_filterIdentity::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.id]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
							Password
					</template>
					<template>[[item.password]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								aria-label="Product"
								path="product"
								value="[[_filterBalance]]">
							<input
									slot="filter"
									placeholder="Product"
									value="{{_filterBalance::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.product]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
							Enabled
					</template>
					<template>[[item.enabled]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
							Multisession
					</template>
					<template>[[item.multisession]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="showAddModal">
				</paper-fab>
			</div>
			<iron-ajax
					id="getSubscriberAjax"
					url="/serviceInventoryManagement/v2/service"
					rejectWithRequest>
			</iron-ajax>
			<iron-ajax
					id="getProductInventoryAjax"
					on-response="_getProductResponse"
					on-error="_getProductError">
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
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterIdentity: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterBalance: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('subscriberGrid');
		grid.dataProvider = this._getSub;
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.subscriberGrid.selectedItems = item ? [item] : [];
		} else {
			this.$.subscriberGrid.selectedItems = [];
		}
	}

	_getSub(params, callback) {
		var grid = this; 
		var serviceList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-sub-list');
		var ajax = serviceList.shadowRoot.getElementById('getSubscriberAjax'); 
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "id" || param.path == "product";
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
			if (request) {
				serviceList.etag = request.xhr.getResponseHeader('ETag');
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
					return characteristic.name == "username";
				}
				for (var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					if(request.response[index].product) {
						newRecord.product = request.response[index].product;
					} else {
						newRecord.product = document.getElementById("subscriberAdd").productId;
					}
					if(request.response[index].attributes) {
						request.response[index].attributes.forEach(
							function(attrObj) {
								newRecord[attrObj.name] = attrObj.value;
							}
						);
					}
					function checkPassword(pass) {
						return pass.name == "servicePassword";
					}
					var indexPass = request.response[index].serviceCharacteristic.findIndex(checkPassword);
					if(indexPass != -1) {
						newRecord.password = request.response[index].serviceCharacteristic[indexPass].value;
					}
					newRecord.enabled = request.response[index].isServiceEnabled;
					function checkMultisession(multi) {
						return multi.name == "multiSession";
					}
					var indexMulti = request.response[index].serviceCharacteristic.findIndex(checkMultisession);
					if(indexMulti != -1) {
						newRecord.multisession = request.response[index].serviceCharacteristic[indexMulti].value;
					}
					vaadinItems[index] = newRecord;
				}
			callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			serviceList.etag = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = error;
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
				if (serviceList.etag && params.page > 0) {
					ajax.headers['If-Range'] = serviceList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (serviceList.etag && params.page > 0) {
				ajax.headers['If-Range'] = serviceList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('subscriberGrid');
		grid.size = 0;
	}

	showAddModal(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-sub-add').shadowRoot.getElementById('addServiceModal').open();
	}
}

window.customElements.define('sig-sub-list', subList);
