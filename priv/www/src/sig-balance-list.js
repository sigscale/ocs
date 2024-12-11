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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js'

class balanceList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="balanceGrid"
					loading="{{loading}}"
					theme="no-border">
				<vaadin-grid-column
						width="24ex">
					<template class="header">
						<vaadin-grid-filter
								aria-label="date"
								path="date"
								value="{{filterTime}}">
							<input
									slot="filter"
									placeholder="Time Stamp"
									value="{{filterTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.date]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						id="check"
						width="10ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="Type"
								path="type"
								value="{{filterType}}">
							<input
									slot="filter"
									placeholder="Type"
									value="{{filterType::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.type]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						id="check1"
						width="14ex">
					<template class="header">
						<vaadin-grid-filter
								aria-label="product"
								path="product"
								value="{{filterProduct}}">
							<input
									slot="filter"
									placeholder="Product"
									value="{{filterProduct::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.product]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						id="check2"
						width="12ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="bucket"
								path="bucket"
								value="{{filterBucket}}">
							<input
									slot="filter"
									placeholder="Bucket"
									value="{{filterBucket::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.bucket]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						id="check3"
						width="13ex"
						flex-grow="2">
					<template class="header">
						Amount
					</template>
					<template>[[item.amount]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
							<div class="grouptitle">Remain Amount</div>
					</template>
					<vaadin-grid-column
							id="check6"
							width="12ex">
						<template class="header">
							Before
						</template>
						<template>[[item.amountBefore]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column
							id="check5"
							width="10ex"
							flex-grow="2">
						<template class="header">
							After
						</template>
						<template>[[item.amountAfter]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<iron-ajax id="getBalanceAjax"
					url="/ocs/v1/log/balance"
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
			filterTime: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterType: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterProduct: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterBucket: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterAmount: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterAfter: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterBefore: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('balanceGrid');
		grid.dataProvider = this._getBalances;
	}

	_getBalances(params, callback) {
		var grid = this; 
		var balanceList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-balance-list');
		var ajax = balanceList.shadowRoot.getElementById('getBalanceAjax');
		delete ajax.params['date'];
		delete ajax.params['filter'];
		function checkDate(filter) {
			return filter.path == "date";
		}
		var dateFilter = params.filters.find(checkDate);
		if (dateFilter && dateFilter.value) {
			ajax.params['date'] = dateFilter.value;
		}
		function checkHead(filter) {
			return filter.path == "type" || filter.path == "bucket"
				|| filter.path == "product";
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if (filter.value) {
				if(ajax.params['filter']) {
					ajax.params['filter'] += "]," + filter.path + ".like=[" + filter.value;
				} else {
					ajax.params['filter'] = "\"[{" + filter.path + ".like=[" + filter.value;
				}
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}

		var handleAjaxResponse = function(request) {
			if (request) {
				balanceList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + (grid.pageSize * 2);
				}
				var vaadinItems = new Array();
				var results = request.response;
				for (var index in results) {
					var balanceLog = new Object();
					balanceLog.date = results[index].date;
					balanceLog.type = results[index].type;
					balanceLog.amount = results[index].amount.amount;
					balanceLog.bucket = results[index].bucketBalance.id;
					balanceLog.amountBefore = results[index].amountBefore.amount;
					balanceLog.amountAfter = results[index].amountAfter.amount;
					balanceLog.product = results[index].product.id;
					vaadinItems[index] = balanceLog;
				}
				callback(vaadinItems);
			}
		}
		var handleAjaxError = function(event) {
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
		}
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (balanceList.etag && params.page > 0) {
					ajax.headers['If-Range'] = balanceList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (balanceList.etag && params.page > 0) {
				ajax.headers['If-Range'] = balanceList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('balanceGrid');
		grid.size = 0;
	}
}

window.customElements.define('sig-balance-list', balanceList);
