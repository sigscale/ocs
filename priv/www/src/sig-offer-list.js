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
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-toast/paper-toast.js';
import './style-element.js'

class offerList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="offerGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="name"
								path="id"
								value="{{_filterName}}">
							<input
									slot="filter"
									placeholder="Name"
									value="{{_filterName::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.id]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="description"
								path="description"
								value="{{_filterDescription}}">
							<input
									slot="filter"
									placeholder="Description"
									value="{{_filterDescription::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="startDate"
								path="startDate"
								value="{{_filterStart}}">
							<input
									slot="filter"
									placeholder="Start Date"
									value="{{_filterStart::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.startDate]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="endDate"
								path="endDate"
								value="{{_filterEnd}}">
							<input
									slot="filter"
									placeholder="End Date"
									value="{{_filterEnd::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.endDate]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="status"
								path="status"
								value="{{_filterStatus}}">
							<input
									slot="filter"
									placeholder="Status"
									value="{{_filterStatus::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.status]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						<vaadin-grid-filter
								id="filter"
								aria-label="price"
								path="price"
								value="{{_filterPrice}}">
							<input
									slot="filter"
									placeholder="Price"
									value="{{_filterPrice::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.price]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap = "showAddOfferModal">
				</paper-fab>
			</div>
			<paper-toast id="getOfferToast">
			</paper-toast>
			<iron-ajax id="getProductAjax"
					url="/catalogManagement/v2/productOffering"
					rejectWithRequest>
			</iron-ajax>
			<iron-ajax id="getTableAjax"
					on-response="_getTableResponse"
					on-error="_getTableError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			offers: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			tables: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
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
			_filterName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterDescription: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterStart: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterEnd: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterStatus: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterPrice: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activeItemChanged(item) {
		if(item) {
			this.$.offerGrid.selectedItems = item ? [item] : [];
			document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-update').initialize(item);
		} else {
			this.$.offerGrid.selectedItems = [];
		}
	}


	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('offerGrid');
		var ajax1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list').shadowRoot.getElementById('getTableAjax');
		ajax1.url = "/catalogManagement/v2/pla";
		ajax1.generateRequest();
		grid.dataProvider = this._getOffers;
	}

	_getOffers(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var offerList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list');
		var ajax = offerList.shadowRoot.getElementById("getProductAjax");
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
		var handleAjaxResponse = function(request) {
			if (request) {
				offerList.etag = request.xhr.getResponseHeader('ETag');
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
					return characteristic.name == "id";
				}
				for (var index in request.response) {
					var newRecord = new Object();
					function checkExist(name) {
						return name == request.response[index].name;
					}
					if(!offerList.offers.some(checkExist)) {
						offerList.push('offers', request.response[index].name);
					}
					newRecord.id = request.response[index].name;
					newRecord.description = request.response[index].description;
					newRecord.productSpecification = request.response[index].productSpecification;
					if(request.response[index].validFor && request.response[index].validFor != "") {
						if(request.response[index].validFor.startDateTime
									&& request.response[index].validFor.startDateTime != ""){
							newRecord.startDate = request.response[index].validFor.startDateTime.split("T")[0];
						}
						if(request.response[index].validFor.endDateTime
									&& request.response[index].validFor.endDateTime != ""){
							newRecord.endDate = request.response[index].validFor.endDateTime.split("T")[0];
						}
					}
					newRecord.lifecycleStatus = request.response[index].status;
					if(request.response[index].productOfferingPrice && request.response[index].productOfferingPrice != "")
					{
						function getNames(price) {
							return price.name;
						}
						newRecord.price = request.response[index].productOfferingPrice.map(getNames).join(", ");
						newRecord.prices = request.response[index].productOfferingPrice;
					}
					newRecord.bundledProductOffering = request.response[index].bundledProductOffering;
					newRecord.prodSpecCharValueUse = request.response[index].prodSpecCharValueUse;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			offerList.etag = null;
			this.$.getOfferToast.text = "Error";
			this.$.getOfferToast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (offerList.etag && params.page > 0) {
				ajax.headers['If-Range'] = offerList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			return ajax.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (offerList.etag && params.page > 0) {
				ajax.headers['If-Range'] = offerList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_getTableResponse(event) {
		var grid = this.shadowRoot.getElementById('offerGrid');
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

	_getTableError(event) {
		this.shadowRoot.getElementById('offerGrid').size = 0;
		cbProduct([]);
		if (!lastItem && event.detail.request.xhr.status != 416) {
			this.$.getOfferToast.text = "Error";
			this.$.getOfferToast.open();
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('offerGrid');
		grid.size = 0;
	}

	showAddOfferModal() {
		var offerList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list');
		offerList.shadowRoot.getElementById("getProductAjax").generateRequest();
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-add').shadowRoot.getElementById('addProductModal').open();
	}
}

window.customElements.define('sig-offer-list', offerList);
