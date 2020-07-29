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
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js'

class accountingList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="accountingGrid"
					loading="{{loading}}">
				<vaadin-grid-column width="24ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="time stamp"
								path="date"
								value="{{filterTimeStamp}}">
							<input
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
									placeholder="Type"
									value="{{filterType::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.type]]</template>
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
				newRecord.date = request.response[index].date;
				function checkChar2(characteristic) {
					return characteristic.name == "nasIdentifier";
				}
				var index2 = request.response[index].usageCharacteristic.findIndex(checkChar2);
				if(index2 != -1) {
					newRecord.nasIdentifier = request.response[index].usageCharacteristic[index2].value;
				}
				function checkChar3(characteristic) {
					return characteristic.name == "acctSessionTime";
				}
				var index3 = request.response[index].usageCharacteristic.findIndex(checkChar3);
				if(index3 != -1) {
					newRecord.acctSessiontime = request.response[index].usageCharacteristic[index3].value;
				}
				function checkChar4(characteristic) {
					return characteristic.name == "outputOctets";
				}
				var index4 = request.response[index].usageCharacteristic.findIndex(checkChar4);
				if(index4 != -1) {
					newRecord.acctOutputoctets = request.response[index].usageCharacteristic[index4].value;
				}
				function checkChar5(characteristic) {
					return characteristic.name == "inputOctets";
				}
				var index5 = request.response[index].usageCharacteristic.findIndex(checkChar5);
				if(index5 != -1) {
					newRecord.acctInputoctets = request.response[index].usageCharacteristic[index5].value;
				}
				function checkChar6(characteristic) {
					return characteristic.name == "totalOctets";
				}
				var index6 = request.response[index].usageCharacteristic.findIndex(checkChar6);
				if(index6 != -1) {
					newRecord.acctTotaloctets = request.response[index].usageCharacteristic[index6].value;
				}
				function checkChar7(characteristic) {
					return characteristic.name == "username";
				}
				var username1 = request.response[index].usageCharacteristic.find(checkChar7);
				if (username1 != undefined) {
					newRecord.username = username1.value;
				}
				function checkChar8(characteristic) {
					return characteristic.name == "type";
				}
				var index8 = request.response[index].usageCharacteristic.findIndex(checkChar8);
				if (index8 != -1) {
					newRecord.type = request.response[index].usageCharacteristic[index8].value;
				}
				for(var indexTax in request.response[index].ratedProductUsage) {
					var taxObj = request.response[index].ratedProductUsage[indexTax].taxExcludedRatingAmount;
					if(taxObj) {
						newRecord.prices = taxObj;
					}
				}
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

