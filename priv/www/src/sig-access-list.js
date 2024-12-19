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
import '@polymer/paper-button/paper-button.js';
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js'


class accessList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid 
					id="accessGrid"
					loading="{{loading}}"
					theme="no-border">
				<vaadin-grid-column-group>
					<vaadin-grid-column width="24ex">
						<template class="header">
							<vaadin-grid-filter
									aria-label="time stamp"
									path="date"
									value="{{filterAccessTimeStamp}}">
								<input
										slot="filter"
										placeholder="Time Stamp"
										value="{{filterAccessTimeStamp::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.date]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="10ex" flex-grow="1">
						<template class="header">
							ClientAddress
						</template>
						<template>[[item.clientAddress]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="1ex" flex-grow="3">
						<template class="header">
							<vaadin-grid-filter
									aria-label="clientIdentity"
									path="nasIdentifier"
									value="{{filterclientIdentity}}">
								<input
										slot="filter"
										placeholder="Client Identity"
										value="{{filterclientIdentity::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.nasIdentifier]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="20ex">
						<template class="header">
							<vaadin-grid-filter
									aria-label="calledStation"
									path="calledStationId"
									value="{{filtercalledStation}}">
								<input
										slot="filter"
										placeholder="Called Station"
										value="{{filtercalledStation::input}}"
										focus-target>
							</vaadin-grid-filter>
						</template>
						<template>[[item.calledStationId]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="10ex" flex-grow="4">
						<template class="header">
							<vaadin-grid-filter
									aria-label="userName"
									path="User Name"
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
					<vaadin-grid-column width="8ex" flex-grow="0">
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
					<template class="footer">Total: {{totalItems}}</template>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<iron-ajax id="getAccess"
					url="/usageManagement/v1/usage"
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
			filterAccessTimeStamp: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filterclientIdentity: {
				type: Boolean,
				observer: '_filterChanged'
			},
			filtercalledStation: {
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
			},
			totalItems: {
				type: String,
				notify: false
			}
		}
	}

	refreshAccess() {
		this.etag = null;
		delete this.$.getAccess.headers['If-Range'];
		delete this.$.getAccess.params['filter'];
		this.filterAccessTimeStamp = null;
		this.filterClientAddress = null;
		this.filterclientIdentity = null;
		this.filtercalledStation = null;
		this.filterUserName = null;
		this.filterType = null
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('accessGrid');
		grid.dataProvider = this._getAccess;
	}

	_getAccess(params, callback) {
		var grid = this;
		var accessList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-access-list');
		var ajax = accessList.shadowRoot.getElementById('getAccess');
		delete ajax.params['date'];
		delete ajax.params['clientAddress'];
		delete ajax.params['nasIdentifier'];
		delete ajax.params['calledStationId'];
		delete ajax.params['filter'];
		ajax.params['type'] = "AAAAccessUsage";
		function checkHead(param) {
			return param.path == "date" || param.path == "type" || param.path == "status";
		}
		var head;
		params.filters.filter(checkHead).forEach(function(filter) {
			if (filter.value) {
				ajax.params[filter.path] = filter.value;
			}
		});
		function checkChar(param) {
			return param.path != "date" && param.path != "type" && param.path != "status";
		}
		params.filters.filter(checkChar).forEach(function(filter) {
			if (filter.value) {
				if (!ajax.params['filter']) {
					ajax.params['filter'] = "\"[{usageCharacteristic.contains=[";
				} else {
					ajax.params['filter'] += ",";
				}
				ajax.params['filter'] += "{name=" + filter.path + ",value.like=[" + filter.value + "%]}";
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		var handleAjaxResponse = function(request) {
			if (request) {
				accessList.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
					accessList.totalItems = range1[1]
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				function checkChar(characteristic){
					return characteristic.name == "id";
				}
				for (var index in request.response) {
					var newRecord = new Object();
					newRecord.date = request.response[index].date;
					function checkChar1(characteristic) {
						return characteristic.name == "clientAddress";
					}
					var index1 = request.response[index].usageCharacteristic.findIndex(checkChar1);
					if (index1 != -1) {
						newRecord.clientAddress = request.response[index].usageCharacteristic[index1].value;
					}
					function checkChar2(characteristic) {
						return characteristic.name == "nasIdentifier";
					}
					var index2 = request.response[index].usageCharacteristic.findIndex(checkChar2);
					if (index2 != -1) {
						newRecord.nasIdentifier = request.response[index].usageCharacteristic[index2].value;
					}
					function checkChar3(characteristic) {
						return characteristic.name == "calledStationId";
					}
					var index3 = request.response[index].usageCharacteristic.findIndex(checkChar3);
					if (index3 != -1) {
						newRecord.calledStationId = request.response[index].usageCharacteristic[index3].value;
					}
					function checkChar4(characteristic) {
						return characteristic.name == "username";
					}
					var username1 = request.response[index].usageCharacteristic.find(checkChar4);
					if (username1 != undefined) {
						newRecord.username = username1.value;
					}
					function checkChar5(characteristic) {
						return characteristic.name == "type";
					}
					var index5 = request.response[index].usageCharacteristic.findIndex(checkChar5);
					if (index5 != -1) {
						newRecord.type = request.response[index].usageCharacteristic[index5].value;
					}
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
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (accessList.etag && params.page > 0) {
					ajax.headers['If-Range'] = accessList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (accessList.etag && params.page > 0) {
				ajax.headers['If-Range'] = accessList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('accessGrid');
		grid.size = 0;
	}
}

window.customElements.define('sig-access-list', accessList);
