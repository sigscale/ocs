/**
 * Copyright 2016 - 2025 SigScale Global Inc.
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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@polymer/iron-icons/iron-icons.js';
import './style-element.js'

class offerList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="offerGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				<vaadin-grid-column-group>
					<vaadin-grid-column>
						<template class="header">
							<vaadin-grid-filter
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
								Description
						</template>
						<template>[[item.description]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
								Start Date
						</template>
						<template>[[item.startDate]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
								End Date
						</template>
						<template>[[item.endDate]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
								Status
						</template>
						<template>[[item.lifecycleStatus]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column>
						<template class="header">
								Price
						</template>
						<template>[[item.price]]</template>
					</vaadin-grid-column>
					<template class="footer">Total: {{totalItems}}</template>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="showAddOfferModal">
				</paper-fab>
			</div>
			<iron-ajax
					id="getProductOffersAjax"
					url="/catalogManagement/v2/productOffering"
					headers='{"Accept": "application/json, application/problem+json"}'
					rejectWithRequest>
			</iron-ajax>
			<iron-ajax
					id="getPrefixTableAjax"
					url="/resourceInventoryManagement/v1/resource"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_getPrefixTableResponse"
					on-error="_getPrefixTableError">
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
				notify: true
			},
			_filterName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			totalItems: {
				type: String,
				notify: false
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('offerGrid');
		var ajax1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list').shadowRoot.getElementById('getPrefixTableAjax');
		ajax1.params['resourceSpecification.id'] = '1';
		ajax1.generateRequest();
		grid.dataProvider = this.getOffers;
		grid.cellClassNameGenerator = this._cellClassNameGenerator;
	}

	_cellClassNameGenerator(column, model) {
		if(column !== undefined && model.item.entityClass !== undefined) {
			return model.item.entityClass;
		} else {
			return null;
		}
	}

	getOffers(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var offerList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list');
		var ajax = offerList.shadowRoot.getElementById("getProductOffersAjax");
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
				ajax.params['filter'] += "characteristic.contains=[{name=description" + ",value.like=[" + filter.value + "%]}";
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
					offerList.totalItems = range1[1]
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic){
					return characteristic.name == "id";
				}
				for (var index in request.response) {
					var newRecord = new Object();
					offerList.push('offers', request.response[index].name);
					newRecord.id = request.response[index].name;
					newRecord.description = request.response[index].description;
					newRecord.productSpecification = request.response[index].productSpecification;
					if(request.response[index].validFor && request.response[index].validFor != "") {
						var date = new Date();
						var currentDate = date.toISOString();
						if(request.response[index].validFor.startDateTime
									&& request.response[index].validFor.startDateTime != ""){
							newRecord.startDate = request.response[index].validFor.startDateTime.split("T")[0];
							if(Date.parse(newRecord.startDate) > Date.parse(currentDate)) {
								newRecord.entityClass = "correctable";
							}
						}
						if(request.response[index].validFor.endDateTime
									&& request.response[index].validFor.endDateTime != ""){
							newRecord.endDate = request.response[index].validFor.endDateTime.split("T")[0];
							if(Date.parse(newRecord.endDate) < Date.parse(currentDate)) {
								newRecord.entityClass = "terminal";
							}
						}
					}
					if(request.response[index].lifecycleStatus) {
						newRecord.lifecycleStatus = request.response[index].lifecycleStatus;
						if(request.response[index].lifecycleStatus == "In Study" || request.response[index].lifecycleStatus == "In Design" || request.response[index].lifecycleStatus == "In Test") {
							newRecord.correctable = request.response[index].lifecycleStatus;
							if(newRecord.correctable) {
								newRecord.entityClass = "correctable";
							}
						}
						if(request.response[index].lifecycleStatus == "Rejected" || request.response[index].lifecycleStatus == "Retired" || request.response[index].lifecycleStatus == "Obsolete") {
							newRecord.terminal = request.response[index].lifecycleStatus;
							if(newRecord.terminal) {
								newRecord.entityClass = "terminal";
							}
						}
					}
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
			}
		};
		var handleAjaxError = function(error) {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
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

	_getPrefixTableResponse(event) {
		var grid = this.shadowRoot.getElementById('offerGrid');
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			this.push('tables', tableRecord);
		}
	}

	_getPrefixTableError(event) {
		this.shadowRoot.getElementById('offerGrid').size = 0;
		cbProduct([]);
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('offerGrid');
		grid.size = 0;
	}

	showAddOfferModal() {
		var offerList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-list');
		offerList.shadowRoot.getElementById("getProductOffersAjax").generateRequest();
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-offer-add').shadowRoot.getElementById('addOfferModal').open();
	}
}

window.customElements.define('sig-offer-list', offerList);
