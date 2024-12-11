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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js'

class productList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="productInventoryGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				</vaadin-grid-column-group>
					<vaadin-grid-column width="15ex" flex-grow="5">
						<template class="header">
							<vaadin-grid-filter
									aria-label="Product Id"
									path="id"
									value="{{_filterProId}}">
								<input
										slot="filter"
										placeholder="Product Id"
										value="{{_filterProId::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.id]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="15ex" flex-grow="5">
						<template class="header">
							<vaadin-grid-filter
									aria-label="Services"
									path="service"
									value="{{_filterIdentity}}">
								<input
										slot="filter"
										placeholder="Services"
										value="{{_filterIdentity::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.service]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column-group>
						<template class="header">
							<div class="grouptitle">Balance</div>
						</template>
						<vaadin-grid-column width="12ex" flex-grow="2">
							<template class="header">
								Cents
							</template>
							<template>
								<div class="cell numeric">[[item.cents]]</div>
							</template>
						</vaadin-grid-column>
						<vaadin-grid-column width="12ex" flex-grow="2">
							<template class="header">
								Bytes
							</template>
							<template>
								<div class="cell numeric">[[item.bytes]]</div>
							</template>
						</vaadin-grid-column>
						<vaadin-grid-column width="12ex" flex-grow="2">
							<template class="header">
								Seconds
							</template>
							<template>
								<div class="cell numeric">[[item.seconds]]</div>
							</template>
						</vaadin-grid-column>
						<vaadin-grid-column width="12ex" flex-grow="2">
							<template class="header">
								Messages
							</template>
							<template>
								<div class="cell numeric">[[item.messages]]</div>
							</template>
						</vaadin-grid-column>
						<template class="footer">Total: {{totalItems}}</template>
					</vaadin-grid-column-group>
					<vaadin-grid-column width="15ex" flex-grow="5">
						<template class="header">
							<vaadin-grid-filter
									aria-label="Offering"
									path="product"
									value="{{_filterOffer}}">
								<input
										slot="filter"
										placeholder="Offering"
										value="{{_filterOffer::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.product]]</template>
					</vaadin-grid-column>
				<vaadin-grid-column-group>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap="showAddProductInven">
				</paper-fab>
			</div>
			<iron-ajax
					id="getProductInventory"
					url="/productInventoryManagement/v2/product/"
					headers='{"Accept": "application/json, application/problem+json"}'
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
			_filterProId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterIdentity: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterOffer: {
				type: Boolean,
				observer: '_filterChanged'
			},
			totalItems: {
				type: String,
				notify: false
			},
		}
	}

	refreshProduct() {
		this.etag = null;
		delete this.$.getProductInventory.headers['If-Range'];
		delete this.$.getProductInventory.params['filter'];
		this._filterProId = null;
		this._filterIdentity = null;
		this._filterOffer = null;
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('productInventoryGrid');
		grid.dataProvider = this._getProduct;
		grid.cellClassNameGenerator = this._cellClassNameGenerator;
	}

	_cellClassNameGenerator(column, model) {
		if(column !== undefined && model.item.entityClass !== undefined) {
			return model.item.entityClass;
		} else {
			return null;
		}
	}

	_getProduct(params, callback) {
		var grid = this;
		var productList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-product-list');
		var ajax = productList.shadowRoot.getElementById("getProductInventory");
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "id" || param.path == "product" || param.path == "service";
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
				productList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
					productList.totalItems = range1[1]
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic){
					return characteristic.name == "id";
				}
				for (var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					newRecord.product = request.response[index].productOffering.name;
					for(var indexPro in request.response[index].balance) {
						if(request.response[index].balance[indexPro].totalBalance) {
							if(request.response[index].balance[indexPro]
										.totalBalance.units == "cents") {
								if(!request.response[index].balance[indexPro].totalBalance.amount) {
											newRecord.cents = request.response[index].balance[indexPro].totalBalance.amount;
								} else {
									newRecord.cents = request.response[index].balance[indexPro].totalBalance.amount;
								}
							}
							if(request.response[index].balance[indexPro]
										.totalBalance.units == "seconds") {
								if(!request.response[index].balance[indexPro].totalBalance.amount) {
											newRecord.seconds = request.response[index].balance[indexPro].totalBalance.amount;
								} else {
									newRecord.seconds = request.response[index].balance[indexPro].totalBalance.amount;
								}
							}
							if(request.response[index].balance[indexPro]
										.totalBalance.units == "octets") {
								if(!request.response[index].balance[indexPro].totalBalance.amount) {
											newRecord.bytes = request.response[index].balance[indexPro].totalBalance.amount;
								} else {
											newRecord.bytes = request.response[index].balance[indexPro].totalBalance.amount;
								}
							}
							if(request.response[index].balance[indexPro]
										.totalBalance.units == "messages") {
								if(!request.response[index].balance[indexPro].totalBalance.amount) {
									newRecord.messages = request.response[index].balance[indexPro].totalBalance.amount;
								} else {
									newRecord.messages = request.response[index].balance[indexPro].totalBalance.amount;
								}
							}
						}
					}
					var serArray = new Array();
					for(var indexSer in request.response[index].realizingService) {
						serArray.push(request.response[index].realizingService[indexSer].id);
						newRecord.service = serArray;
					}
					var date = new Date();
					var currentDate = date.toISOString();
					if(request.response[index].startDate) {
						newRecord.StartDate = request.response[index].startDate;
						if(Date.parse(newRecord.StartDate) > Date.parse(currentDate)) {
							newRecord.entityClass = "correctable";
						}
					}
					if(request.response[index].terminationDate) {
						newRecord.endDate = request.response[index].terminationDate;
						if(Date.parse(newRecord.endDate) < Date.parse(currentDate)) {
							newRecord.entityClass = "terminal";
						}
					}
					if(request.response[index].status){
						if(request.response[index].status == "Pending Active" || request.response[index].status == "Suspended" || request.response[index].status == "Pending Terminate") {
							newRecord.correctable = request.response[index].status;
							if(newRecord.correctable) {
								newRecord.entityClass = "correctable";
							}
						}
						if(request.response[index].status == "Aborted" || request.response[index].status == "Cancelled" || request.response[index].status == "Terminated") {
							newRecord.terminal = request.response[index].status;
							if(newRecord.terminal) {
								newRecord.entityClass = "terminal";
							}
						}
					}
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			}
		}
		var handleAjaxError = function(error) {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (productList.etag && params.page > 0) {
					ajax.headers['If-Range'] = productList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (productList.etag && params.page > 0) {
				ajax.headers['If-Range'] = productList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('productInventoryGrid');
		grid.size = 0;
	}

	showAddProductInven(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-product-add').shadowRoot.getElementById('addProductInvenModal').open();
	}
}

window.customElements.define('sig-product-list', productList);
