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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js'
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import './style-element.js'
import '@polymer/iron-icons/iron-icons.js';

class policyList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="policyGrid"
					loading="{{loading}}"
					active-item="{{activeItem}}"
					theme="no-border">
				<vaadin-grid-column>
					<template class="header">
						Name
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Service Quality
					</template>
					<template>[[item.qos]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Charging Rule
					</template>
					<template>[[item.chargingRule]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						flow
					</template>
					<template>[[item.flow]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Precedence
					</template>
					<template>[[item.Precedence]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddPolicyModal">
				</paper-fab>
			</div>
			<paper-toast
				id="PolicyToast">
			</paper-toast>
			<iron-ajax id="getPolicyAjax"
				url="/resourceInventoryManagement/v1/resource"
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
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			}
		}
	}

	_activeItemChanged(item) {
		//todo
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('policyGrid');
		grid.dataProvider = this._getPolicy;
	}


	_getPolicy(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var policyList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
		var ajax = policyList.shadowRoot.getElementById('getPolicyAjax');
		var handleAjaxResponse = function(request) {
			if(request) {
				policyList.etag = request.xhr.getResponseHeader('ETag');
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
					var newRecord = new Object();
					newRecord.name = request.response[index].name;
					newRecord.service = request.response[index].service;
					newRecord.chargingRule = request.response[index].chargingRule;
					newRecord.flow = request.response[index].flow;
					newRecord.precedence= request.response[index].precedence;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			policyList.etag = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').shadowRoot.getElementById('PolicyToast');
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
			if (policyList.etag && params.page > 0) {
				ajax.headers['If-Range'] = clientList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
				return ajax.generateRequest().completes;
					}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (policyList.etag && params.page > 0) {
				ajax.headers['If-Range'] = clientList.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	showAddPolicyModal(event) {
//Todo
	}
}

window.customElements.define('sig-policy-list', policyList);
