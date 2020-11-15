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
import '@polymer/paper-toast/paper-toast.js';
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
								aria-label="timeStamp"
								path="timeStamp"
								value="{{filterTime}}">
							<input
									placeholder="Time Stamp"
									value="{{filterTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.timeStamp]]</template>
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
						<vaadin-grid-filter
								aria-label="amount"
								path="amount"
								value="{{filterAmount}}">
							<input
									placeholder="Amount"
									value="{{filterAmount::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.amount]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
							<div class="grouptitle">Remain Amount</div>
					</template>
					<vaadin-grid-column
							id="check5"
							width="10ex"
							flex-grow="2">
						<template class="header">
							<vaadin-grid-filter
									aria-label="amountAfter"
									path="amountAfter"
									value="{{filterAfter}}">
								<input
										placeholder="After"
										value="{{filterAfter::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.amountAfter]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column
							id="check6"
							width="12ex">
						<template class="header">
							<vaadin-grid-filter
									aria-label="amountBefore"
									path="amountBefore"
									value="{{filterBefore}}">
								<input
										placeholder="Before"
										value="{{filterBefore::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.amountBefore]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<iron-ajax id="getBalanceAjax"
				url="/ocs/v1/log/balance"
				rejectWithRequest>
			</iron-ajax>
			<paper-toast id="getBalanceToast">
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
		grid.dataProvider = this._getBalanceResponse;
	}

	_getBalanceResponse(params, callback) {
		var grid = this; 
		var balanceList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-balance-list');
		var ajax = balanceList.shadowRoot.getElementById('getBalanceAjax');

		function checkHead(param) {
			return param.path == "timeStamp" || param.path == "type" ||
				param.path == "amount" || param.path == "bucket" ||
				param.path == "amountBefore" || param.path == "amountAfter" ||
				param.path == "product";
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
				grid.size = 100;
				var vaadinItems = new Array();
				var results = request.response;
				for (var index in results) {
					var balanceLog = new Object();
					balanceLog.timeStamp = results[index].date;
					balanceLog.type = results[index].type;
					balanceLog.amount = results[index].amount.amount;
					balanceLog.bucket = results[index].bucketBalance.id;
					balanceLog.amountBefore = results[index].amountBefore.amount;
					balanceLog.amountAfter = results[index].amountAfter.amount;
					balanceLog.product = results[index].product.id;
					vaadinItems[index] = balanceLog;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		}
		var handleAjaxError = function(event) {
			balanceList.etag = null;
			this.$.getBalanceToast.text = "Error";
			this.$.getBalanceToast.open();
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
