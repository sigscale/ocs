/**
 * Copyright 2016 - 2021 SigScale Global Inc.
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
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-icons/iron-icons.js';
import './style-element.js'

class bucketList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="balanceBucketGrid"
					active-item="{{activeItem}}"
					loading="{{loading}}"
					theme="no-border">
				<template class="row-details">
					<dl class="details">
						<template is="dom-if" if="{{item.id}}">
							<dt><b>Bucket Id</b></dt>
							<dd>{{item.id}}</dd>
						</template>
						<template is="dom-if" if="{{item.product}}">
							<dt><b>Product Id</b></dt>
							<dd>{{item.product}}</dd>
						</template>
						<template is="dom-if" if="{{item.startDate}}">
							<dt><b>Start Date</b></dt>
							<dd>{{item.startDate}}</dd>
						</template>
						<template is="dom-if" if="{{item.endDate}}">
							<dt><b>End Date</b></dt>
							<dd>{{item.endDate}}</dd>
						</template>
						<template is="dom-if" if="{{item.amount}}">
							<dt><b>Remained Amount</b></dt>
							<dd>{{item.amount}}</dd>
						</template>
						<template is="dom-if" if="{{item.units}}">
							<dt><b>Units</b></dt>
							<dd>{{item.units}}</dd>
						</template>
					</dl>
					<div class="buttons">
						<paper-button
							raised
							on-tap="_tableDelete"
							class="delete-button">
							Delete
						</paper-button>
					</div>
				</template>
				<vaadin-grid-column width="15ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="Bucket Id"
								path="id"
								value="[[_filterBucketId]]">
							<input
									slot="filter"
									placeholder="Bucket Id"
									value="{{_filterBucketId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.id]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="15ex" flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="Product Id"
								path="product"
								value="[[_filterProductId]]">
							<input
									slot="filter"
									placeholder="Product Id"
									value="{{_filterProductId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
						[[item.product]]
					</template>
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
							<div class="cell numeric">
								[[item.cents]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="12ex" flex-grow="2">
						<template class="header">
								Bytes
						</template>
						<template>
							<div class="cell numeric">
								[[item.remainedAmount]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="12ex" flex-grow="2">
						<template class="header">
								Seconds
						</template>
						<template>
							<div class="cell numeric">
								[[item.seconds]]
							</div>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap = "showAddBucket">
				</paper-fab>
			</div>
			<iron-ajax
					id="getBucketBalance"
					url="/balanceManagement/v1/bucket/"
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
				notify: true,
				observer: '_activeItemChanged'
			},
			_filterBucketId: {
				type: Boolean
			},
			_filterProductId: {
				type: Boolean
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('balanceBucketGrid');
		grid.dataProvider = this._getBuckets;
		grid.cellClassNameGenerator = this._cellClassNameGenerator;
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.balanceBucketGrid;
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item;
			}
			function checkExist(bucket) {
				return bucket.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	_cellClassNameGenerator(column, model) {
		if(column !== undefined && model.item.expired) {
			return "expired";
		} else {
			return null;
		}
	}

	_tableDelete(item) {
		var bucketAdd = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-bucket-add');
		bucketAdd.shadowRoot.getElementById('deleteBucketModal').open();
		bucketAdd.deleteBucketId = item.model.item.id;
	}

	_getBuckets(params, callback) {
		var grid = this;
		var bucketList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-bucket-list');
		var ajax = bucketList.shadowRoot.getElementById("getBucketBalance");
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "id" || param.path == "product";
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if (filter.value) {
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
				bucketList.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.href = request.response[index].href;
					newRecord.id = request.response[index].id;
					if(request.response[index].product) {
						if(request.response[index].product.id) {
							newRecord.product = request.response[index].product.id;
						}
						if(request.response[index].product.href) {
							newRecord.productHref = request.response[index].product.href;
						}
					}
					if(request.response[index].remainedAmount) {
						if(request.response[index].remainedAmount.units) {
							newRecord.units = request.response[index].remainedAmount.units;
						}
						if(request.response[index].remainedAmount.amount) {
							newRecord.amount = request.response[index].remainedAmount.amount;
						}
						if(request.response[index].remainedAmount.units == "cents") {
							newRecord.cents = request.response[index].remainedAmount.amount;
						}
						if(request.response[index].remainedAmount.units == "seconds") {
							var Str = request.response[index].remainedAmount.amount;
							if(Str.includes("s")){
								var NewStrSec = Str.substring(0, Str.length - 1);
								newRecord.seconds = NewStrSec;
							}
						}
						if(request.response[index].remainedAmount.units == "octets") {
							var Str = request.response[index].remainedAmount.amount;
							if(Str.includes("b")){
								var NewStr = Str.substring(0, Str.length - 1);
								newRecord.remainedAmount = NewStr;
							}
						}
					}
					if(request.response[index].validFor) {
						if(request.response[index].validFor.startDateTime) {
							newRecord.startDate = request.response[index].validFor.startDateTime;
						}
						if(request.response[index].validFor.endDateTime) {
							newRecord.endDate = request.response[index].validFor.endDateTime;
							var date = new Date();
							var currentDate = date.toISOString();
							if(Date.parse(newRecord.endDate) < Date.parse(currentDate)) {
								newRecord.expired = true;
							} else {
								newRecord.expired = false;
							}
						}
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
			bucketList.etag = null;
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
				if (bucketList.etag && params.page > 0) {
					ajax.headers['If-Range'] = bucketList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (bucketList.etag && params.page > 0) {
				ajax.headers['If-Range'] = bucketList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	showAddBucket(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-bucket-add').shadowRoot.getElementById('addBucketModal').open();
	}
}

window.customElements.define('sig-bucket-list', bucketList);
