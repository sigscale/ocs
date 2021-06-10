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
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js'
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-button/paper-button.js';
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import './style-element.js'

class accountingList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="accountingGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}">
				<template class="row-details">
					<paper-tabs
							class="details"
							selected="{{selectedTab}}">
						<paper-tab>
							General
						</paper-tab>
						<paper-tab>
							Characteristics
						</paper-tab>
						<paper-tab>
							Specification
						</paper-tab>
					</paper-tabs>
					<iron-pages
							selected="{{selectedTab}}">
						<div>
							<dl class="details">
								<template is="dom-if" if="{{item.id}}">
									<dt><b>Id</b></dt>
									<dd>{{item.id}}</dd>
								</template>
								<template is="dom-if" if="{{item.date}}">
									<dt><b>Date</b></dt>
									<dd>{{item.date}}</dd>
								</template>
								<template is="dom-if" if="{{item.status}}">
									<dt><b>Status</b></dt>
									<dd>{{item.status}}</dd>
								</template>
								<template is="dom-if" if="{{item.type}}">
									<dt><b>Type</b></dt>
									<dd>{{item.type1}}</dd>
								</template>
								<template is="dom-if" if="{{item.href}}">
									<dt><b>Href</b></dt>
									<dd>{{item.href}}</dd>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.usageCharacteristic}}">
								<table class="details">
									<tr>
										<th>Name</th>
										<th>Value</th>
									</tr>
									<template is="dom-repeat" items="{{item.usageCharacteristic}}" as="char">
										<tr>
											<td>{{char.name}}</td>
											<td>{{char.value}}</td>
										</tr>
									</template>
								</table>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.usageSpecificationId}}">
								<dt><b>Id</b></dt>
								<dd>{{item.usageSpecificationId}}</dd>
							</template>
							<template is="dom-if" if="{{item.usageSpecificationName}}">
								<dt><b>Name</b></dt>
								<dd>{{item.usageSpecificationName}}</dd>
							</template>
							<template is="dom-if" if="{{item.usageSpecificationHref}}">
								<dt><b>Href</b></dt>
								<dd>{{item.usageSpecificationHref}}</dd>
							</template>
						</div>
					</iron-pages>
				</template>
				<vaadin-grid-column width="24ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="time stamp"
								path="date"
								value="{{filterTimeStamp}}">
							<input
									slot="filter"
									placeholder="Time Stamp"
									value="{{filterTimeStamp::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.date]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="8">
					<template class="header">
						<vaadin-grid-filter
								aria-label="client identity"
								path="nasIdentifier"
								value="{{filterclientIdentityAcc}}">
							<input
									slot="filter"
									placeholder="Client Identity"
									value="{{filterclientIdentityAcc::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.nasIdentifier]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
						<div class="grouptitle">Seconds</div>
					</template>
					<vaadin-grid-column width="15ex" flex-grow="1">
						<template class="header">
							<vaadin-grid-filter
									aria-label="duration"
									path="acctSessiontime"
									value="{{filteracctSessiontime}}">
								<input
										slot="filter"
										placeholder="Duration"
										value="{{filteracctSessiontime::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.acctSessiontime]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column-group>
					<template class="header">
						<div class="grouptitle">Bytes</div>
					</template>
					<vaadin-grid-column width="10ex" flex-grow="1">
						<template class="header">
							<vaadin-grid-filter
									aria-label="out"
									path="acctOutputoctets"
									value="{{filterout}}">
								<input
										slot="filter"
										placeholder="Out"
										value="{{filterout::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.acctOutputoctets]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="10ex" flex-grow="1">
						<template class="header">
							<vaadin-grid-filter
									aria-label="in"
									path="acctInputoctets"
									value="{{filterin}}">
								<input
										slot="filter"
										placeholder="In"
										value="{{filterin::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.acctInputoctets]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="10ex" flex-grow="1">
						<template class="header">
							<vaadin-grid-filter
									aria-label="total"
									path="acctTotaloctets"
									value="{{filtertotal}}">
								<input
										slot="filter"
										placeholder="Total"
										value="{{filtertotal::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.acctTotaloctets]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="price"
								path="prices"
								value="{{filterPrices}}">
							<input
									slot="filter"
									placeholder="Price"
									value="{{filterPrices::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.prices]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="user name"
								path="username"
								value="{{filterUserName}}">
							<input
									slot="filter"
									placeholder="User Name"
									value="{{filterUserName::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.username]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="type"
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
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="msisdn"
								path="msisdn"
								value="{{filterMsisdn}}">
							<input
									slot="filter"
									placeholder="MSISDN"
									value="{{filterMsisdn::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.msisdn]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="8ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="imsi"
								path="imsi"
								value="{{filterImsi}}">
							<input
									slot="filter"
									placeholder="IMSI"
									value="{{filterImsi::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.imsi]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax id="getAccounting"
					url="/usageManagement/v1/usage"
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
			filterTimeStamp: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterclientIdentityAcc: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filteracctSessiontime: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterout: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterin: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filtertotal: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterPrices: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterUserName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterType: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.accountingGrid;
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item;
			}
			function checkExist(specification) {
				return specification.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	refreshAccounting() {
		this.etag = null;
		delete this.$.getAccounting.headers['If-Range'];
		this.filterTimeStamp = null;
		this.filterclientIdentityAcc = null;
		this.filteracctSessiontime = null;
		this.filterout = null;
		this.filterin = null;
		this.filtertotal = null;
		this.filterUserName = null;
		this.filterPrices = null;
		this.filterType = null;
		this.filterMsisdn = null;
		this.filterImsi = null
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('accountingGrid');
		grid.dataProvider = this._getAccounting;
	}

	_getAccounting(params, callback) {
		var grid = this;
		var accountingList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-accounting-list');
		var ajax = accountingList.shadowRoot.getElementById('getAccounting');
		delete ajax.params['date'];
		delete ajax.params['nasIdentifier'];
		delete ajax.params['acctSessiontime'];
		delete ajax.params['acctOutputoctets'];
		delete ajax.params['acctInputoctets'];
		delete ajax.params['acctTotaloctets'];
		delete ajax.params['filter'];
		ajax.params['type'] = "AAAAccountingUsage";
		function checkHead(param) {
			return param.path == "date" || param.path == "status";
		}
		var head;
		params.filters.filter(checkHead).forEach(function(filter) {
			if (filter.value) {
				ajax.params[filter.path] = filter.value;
			}
		});
		function checkChar(param) {
			return param.path != "date" && param.path != "status";
		}
		params.filters.filter(checkChar).forEach(function(filter) {
			if (filter.value) {
				if (!ajax.params['filter']) {
					ajax.params['filter'] = "\"[{usageCharacteristic.contains=[";
				} else {
					ajax.params['filter'] += ",";
				}
				if(isNaN(filter.value)) {
					ajax.params['filter'] += "{name=" + filter.path + ",value.like=[" + filter.value + "%]}";
				} else {
					ajax.params['filter'] += "{name=" + filter.path + ",value.gte=" + filter.value + "%}";
				}
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		var handleAjaxResponse = function(request) {
			if (request) {
				accountingList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				for (var index in request.response) {
					var newRecord = new Object();
					newRecord.id = request.response[index].id;
					newRecord.date = request.response[index].date;
					newRecord.status = request.response[index].status;
					newRecord.type1 = request.response[index].type;
					newRecord.href = request.response[index].href;
					newRecord.usageCharacteristic = request.response[index].usageCharacteristic;
					function checkChar(characteristic){
						return characteristic.name == "msisdn";
					}
					var msi = request.response[index].usageCharacteristic.find(checkChar);
					if(msi != undefined) {
						newRecord.msisdn = msi.value;
					}
					function checkChar1(characteristic1){
						return characteristic1.name == "imsi";
					}
					var imsi = request.response[index].usageCharacteristic.find(checkChar1);
					if(imsi != undefined) {
						newRecord.imsi = imsi.value;
					}
					newRecord.usageSpecificationId = request.response[index].usageSpecification.id;
					newRecord.usageSpecificationHref = request.response[index].usageSpecification.href;
					newRecord.usageSpecificationName = request.response[index].usageSpecification.name;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		}
		var handleAjaxError = function(error) {
			accountingList.etag = null;
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
				if (accountingList.etag && params.page > 0) {
					ajax.headers['If-Range'] = accountingList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (accountingList.etag && params.page > 0) {
				ajax.headers['If-Range'] = accountingList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('accountingGrid');
		grid.size = 0;
	}
}

window.customElements.define('sig-accounting-list', accountingList);
