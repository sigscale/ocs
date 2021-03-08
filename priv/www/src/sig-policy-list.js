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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js'
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
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
							Relationship
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
								<template is="dom-if" if="{{item.name}}">
									<dt><b>Name</b></dt>
									<dd>{{item.name}}</dd>
								</template>
								<template is="dom-if" if="{{item.href}}">
									<dt><b>Href</b></dt>
									<dd>{{item.href}}</dd>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceCharacteristic}}">
								<table class="details">
									<tr>
										<th>Name</th>
										<th>Value</th>
									</tr>
									<tr>
										<td>Name</td>
										<td>{{item.name}}</td>
									</tr>
									<tr>
										<td>qosInformation</td>
										<td>{{item.qos}}</td>
									</tr>
									<tr>
										<td>chargingRule</td>
										<td>{{item.chargingRule}}</td>
									</tr>
									<tr>
										<td>flowInformationUp1</td>
										<td>{{item.flowUp}}</td>
									</tr>
									<tr>
										<td>flowInformationDown1</td>
										<td>{{item.flowDown}}</td>
									</tr>
									<tr>
										<td>precedence</td>
										<td>{{item.precedence}}</td>
									</tr>
								</table>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceRelationship}}">
								<table class="details">
									<tr>
										<th>Id</th>
										<th>Href</th>
										<th>Name</th>
										<th>Relationship Type</th>
									</tr>
									<template is="dom-repeat" items="{{item.resourceRelationship}}" as="rel">
										<tr>
											<td>{{rel.id}}</td>
											<td>{{rel.href}}</td>
											<td>{{rel.name}}</td>
											<td>{{rel.type}}</td>
										</tr>
									</template>
								</table>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.resourceSpecification}}">
								<table class="details">
									<tr>
										<th>Id</th>
										<th>Name</th>
									</tr>
									<tr>
										<td>{{item.resourceSpecification.id}}</td>
										<td>{{item.resourceSpecification.name}}</td>
									</tr>
								</table>
							</template>
						</div>
					</iron-pages>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_up">
								Update
							</paper-button>
							<paper-button
									raised
									on-tap="tableDelete"
									class="delete-button">
								Delete
							</paper-button>
						</div>
				</template>
				<vaadin-grid-column>
					<template class="header">
						Id
					</template>
					<template>[[item.id]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Name
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						QOS
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
						Flow Up
					</template>
					<template>[[item.flowUp]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Flow Down
					</template>
					<template>[[item.flowDown]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Precedence
					</template>
					<template>[[item.precedence]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<paper-dialog class="dialog" id="tableList">
				<app-toolbar>
					List of Tables
				</app-toolbar>
				<template is="dom-repeat" items="[[tables]]">
					<paper-item
							id="pagePolicy"
							class="menuitem"
							on-focused-changed="tableSelection">
						<iron-icon icon ="icons:view-list" item-icon></iron-icon>
							{{item.name}}
					</paper-item>
				</template>
				<div class="buttons">
					<paper-button
							raised
							id="tabOkButton"
							disabled
							on-tap="tableOk"
							class="submit-button">
						Ok
					</paper-button>
					<paper-button
							dialog-dismiss
							class="cancel-button">
						Cancel
					</paper-button>
					<paper-button
							raised
							on-tap="tableAdd"
							class="submit-button">
						Add
					</paper-button>
					<paper-button
							raised
							on-tap="tableDelete"
							class="delete-button">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<div class="add-button">
				<paper-fab
					icon="add"
					on-tap = "showAddPolicyModal">
				</paper-fab>
			</div>
			<paper-toast
				id="PolicyToast">
			</paper-toast>
			<iron-ajax id="getPolicyContentAjax"
					on-response="_getPolicyContentResponse"
					on-error="_getPolicyContentError">
			</iron-ajax>
			<iron-ajax id="getPolicyAjax"
				url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=3"
				on-response="_getPolicyResponse"
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
			tables: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			activeItem: {
				type: Object,
				notify: true,
				observer: '_activeItemChanged'
			}
		}
	}

	ready() {
		super.ready();
		var ajax1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').shadowRoot.getElementById('getPolicyAjax');
		ajax1.generateRequest();
		this.$.tableList.open();
	}

	tableOk() {
		var grid = this.shadowRoot.getElementById('policyGrid');
		grid.dataProvider = this._getPolicy;
		this.$.tableList.close();
	} 

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.policyGrid;
			var current;
			if(item == null) {
				current = last;
				this.$.policyGrid.selectedItems = item ? [item] : [];
			} else {
				current = item;
				this.$.policyGrid.selectedItems = [];
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

	_up() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-update').shadowRoot.getElementById('updatePolicyModal').open();
	}

	_getPolicyResponse(event) {
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.name = results[indexTable].name;
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			tableRecord.description = results[indexTable].description;
			tableRecord.plaSpecId = results[indexTable].plaSpecId;
			this.push('tables', tableRecord);
		}
	}

	tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').table = e.model.item.name;
			document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').tableId = e.model.item.id;
			this.$.tabOkButton.disabled = false;
		} else {
			this.$.tabOkButton.disabled = true;
		}
	}

	_getPolicy(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var policyList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
		var ajax = policyList.shadowRoot.getElementById('getPolicyContentAjax');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=4&resourceRelationship.name=" + policyList.table;
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
					var tabObj = new Object();
					tabObj.id = request.response[index].id;
					tabObj.resourceCharacteristic = request.response[index].resourceCharacteristic;
					var resChar = request.response[index].resourceCharacteristic;
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "name") {
							tabObj.name = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "qosInformation") {
							var qos1 = resChar[indexRes].value.value.maxRequestedBandwidthDL;
							var qos2 = resChar[indexRes].value.value.maxRequestedBandwidthUL;
							var qos3 = resChar[indexRes].value.value.qosClassIdentifier;
							tabObj.qos = "class:" + qos3 + ", UL:" + qos2 + ", DL:" + qos1;
						}
						if(resChar[indexRes].name == "chargingRule") {
							tabObj.chargingRule = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "flowInformation") {
							for(var indexFl in resChar[indexRes].value.value){
								if(resChar[indexRes].value.value[indexFl].name == "flowInformationUp1") {
									tabObj.flowUp = resChar[indexRes].value.value[indexFl].flowDirection;
								}
								if(resChar[indexRes].value.value[indexFl].name == "flowInformationDown1"){
									tabObj.flowDown = resChar[indexRes].value.value[indexFl].flowDirection;
								}
							}
						}
						if(resChar[indexRes].name == "precedence") {
							tabObj.precedence = resChar[indexRes].value.value;
						}
						if(request.response[index].resourceRelationship) {
							tabObj.resourceRelationship = request.response[index].resourceRelationship;
						}
						if(request.response[index].resourceSpecification) {
							tabObj.resourceSpecification = request.response[index].resourceSpecification;
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

	tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-table-add').shadowRoot.getElementById('addPolicyTableModal').open();
		this.$.tableList.close();
	}

	showAddPolicyModal(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-add').shadowRoot.getElementById('policyAddModal').open();
	}
}

window.customElements.define('sig-policy-list', policyList);
