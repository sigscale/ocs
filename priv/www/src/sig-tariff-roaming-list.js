/**
 * Copyright 2016 - 2022 SigScale Global Inc.
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
import './style-element.js'

class roamingList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="roamingGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				<vaadin-grid-column width="15ex">
					<template class="header">
						<vaadin-grid-filter
								aria-label="prefix"
								path="prefix"
								value="{{_filterPrefix}}">
							<input
									slot="filter"
									placeholder="Prefix"
									value="{{_filterPrefix::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.prefix]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="15ex">
					<template class="header">
							Description
					</template>
					<template>[[item.description]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
               <template class="header">
                  <div class="grouptitle">Initial Period</div>
               </template>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Seconds
						</template>
						<template>
							<div class="cell numeric">
								[[item.seconds]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Rate
						</template>
						<template>
							<div class="cell numeric">
								[[item.rate]]
							</div>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column-group>
               <template class="header">
                  <div class="grouptitle">Additional Period</div>
               </template>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Seconds
						</template>
						<template>
							<div class="cell numeric">
								[[item.seconds]]
							</div>
						</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="15ex">
						<template class="header">
								Rate
						</template>
						<template>
							<div class="cell numeric">
								[[item.rate]]
							</div>
						</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
						icon="add"
						on-tap="showAddPrefixModal">
				</paper-fab>
			</div>
			<iron-ajax
					id="getRoamingRows">
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
			_filterPrefix: {
				type: Boolean,
				observer: '_filterChanged'
			},
			activeTableName: {
				type: String,
				notify: true
			},
			activeTableId: {
				type: String
			},
			activeItem: {
				type: Object,
				notify: true
			}
		}
	}

	ready() {
		super.ready();
		document.body.querySelector('sig-app').viewTitle = 'Tariff: ' + this.activeTableName;
		var grid = this.shadowRoot.getElementById('roamingGrid');
		grid.dataProvider = this._getRoamingTable;
		grid.clearCache();
	}

	_getRoamingTable(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var roamingList = document.body.querySelector('sig-app').shadowRoot.getElementById('roamingList');
		var ajax = roamingList.shadowRoot.getElementById('getRoamingRows');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=6&resourceRelationship.resource.name=" + roamingList.activeTableName;
		delete ajax.params['filter'];
		function checkHead(param) {
			return param.path == "prefix"
		}
		params.filters.filter(checkHead).forEach(function(filter) {
			if(filter.value) {
				if(ajax.params['filter']) {
					ajax.params['filter'] += "]," + "resourceCharacteristic." + filter.path + ".like=[" + filter.value + "%";
				} else {
					ajax.params['filter'] = "\"[{" + "resourceCharacteristic." +filter.path + ".like=[" + filter.value + "%";
				}
			}
		});
		if (ajax.params['filter']) {
			ajax.params['filter'] += "]}]\"";
		}
		var handleAjaxResponse = function(request) {
			if(request) {
				roamingList.etag = request.xhr.getResponseHeader('ETag');
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
					return characteristic.name == "name"
				}
				for (var index in request.response) {
					var tabObj = new Object();
					tabObj.id = request.response[index].id;
					tabObj.resourceCharacteristic = request.response[index].resourceCharacteristic;
					var resChar = request.response[index].resourceCharacteristic;
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "prefix") {
							tabObj.prefix = resChar[indexRes].value;
						}
						if(resChar[indexRes].name == "description") {
							tabObj.description = resChar[indexRes].value;
						}
						if(resChar[indexRes].name == "rate") {
							tabObj.rate = resChar[indexRes].value;
						}
						vaadinItems[index] = tabObj;
					}
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			roamingList.etag = null;
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
				if (roamingList.etag && params.page > 0) {
					ajax.headers['If-Range'] = roamingList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (roamingList.etag && params.page > 0) {
				ajax.headers['If-Range'] = roamingList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('roamingGrid');
		grid.size = 0;
	}

	showAddPrefixModal() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-tariff-rate-add').shadowRoot.getElementById('addPrefixModal').open();
	}
}

window.customElements.define('sig-tariff-roaming-list', roamingList);
