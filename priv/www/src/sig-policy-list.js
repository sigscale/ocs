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
import {} from '@polymer/polymer/lib/elements/dom-if.js';
import {} from '@polymer/polymer/lib/elements/dom-repeat.js'
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import '@polymer/iron-ajax/iron-ajax.js';
import '@polymer/paper-fab/paper-fab.js';
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/iron-collapse/iron-collapse.js';
import '@polymer/iron-icons/iron-icons.js';
import './style-element.js'

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
							QoS Information
						</paper-tab>
						<paper-tab>
							Service Flow
						</paper-tab>
					</paper-tabs>
					<iron-pages
							selected="{{selectedTab}}">
						<div>
							<div>
								<paper-input
										label="Id"
										value="{{polId}}"
										disabled>
								</paper-input>
								<paper-tooltip>
									Internal policy rule identifier.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Name"
										value="{{polName}}">
								</paper-input>
								<paper-tooltip>
									Uniquely identifies a PCRF provided rule within a session, or references a rule predefined at PCEF.
								</paper-tooltip>
							</div>
							<div>
								<paper-toggle-button
										checked="{{predefined}}">
									Predefined
								</paper-toggle-button>
								<paper-tooltip>
									Indicates if the rule is predefined at PCEF.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Precedence"
										value="{{item.precedence}}">
								</paper-input>
								<paper-tooltip>
									Determines the order in which service data flow templates are applied for detection at the PCEF. A rule with a lower value shall be applied before a rule with a higher value.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Charging Key"
										value="{{item.chargingKey}}">
								</paper-input>
								<paper-tooltip>
									Charging Key (Rating Group) used for rating the service associated with the service flow of this rule.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Service Identifier"
										value="{{item.serviceId}}">
								</paper-input>
								<paper-tooltip>
									Identify the service or the service component the service data flow relates to.
								</paper-tooltip>
							</div>
						</div>
						<div>
							<div>
								<paper-input
										label="Max Requested Bandwidth DL"
										value="{{item.maxRequestedBandwidthDL}}">
								</paper-input>
								<paper-tooltip>
									Maximum allowed bit rate for downlink.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="Max Requested Bandwidth UL"
										value="{{item.maxRequestedBandwidthUL}}">
								</paper-input>
								<paper-tooltip>
									Maximum allowed bit rate for uplink.
								</paper-tooltip>
							</div>
							<div>
								<paper-input
										label="QoS Class Identifier (QCI)"
										value="{{item.qosClassIdentifier}}">
								</paper-input>
								<paper-tooltip>
									The authorized QoS for the default EPS bearer.
								</paper-tooltip>
							</div>
						</div>
						<div>
							<template id="flowDomRepeat" is="dom-repeat" items="{{item.flow}}" as="flowitem" mutable-data="true">
								<div class="flowRow">
									<paper-icon-button
											class="minus-icon-button"
											icon="icons:remove-circle-outline"
											on-tap = "_flowMinus">
									</paper-icon-button>
									<div>
										<paper-dropdown-menu
												value="{{flowitem.flowDirection}}"
												no-animations="true"
												label="Flow Direction">
											<paper-listbox
													slot="dropdown-content">
												<paper-item>
													up
												</paper-item>
												<paper-item>
													down
												</paper-item>
												<paper-item>
													both
												</paper-item>
											</paper-listbox>
										</paper-dropdown-menu>
										<paper-tooltip>
											Indicates the direction that a filter is applicable: downlink only, uplink only or both (bidirectional).
										</paper-tooltip>
									</div>
									<div>
										<paper-input
												label="Flow Filter"
												value="{{flowitem.flowDescription}}">
										</paper-input>
										<paper-tooltip>
											Defines a packet filter for an IP flow (e.g. permit out proto ip from all to all).
										</paper-tooltip>
									</div>
								</div>
							</template>
							<paper-icon-button
									class="add-icon-button"
									icon="icons:add-circle-outline"
									on-tap = "_flowPlus">
							</paper-icon-button>
						</div>
					</iron-pages>
					<div class="buttons">
						<paper-button
								raised
								class="submit-button"
								on-tap="_update">
							Update
						</paper-button>
						<paper-button
								raised
								class="delete-button"
								on-tap="_delete">
							Delete
						</paper-button>
					</div>
				</template>
				<vaadin-grid-column width="12ex" flex-grow="2">
					<template class="header">
						Name
					</template>
					<template>[[item.name]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="20ex" flex-grow="2">
					<template class="header">
						QoS
					</template>
					<template>[[item.qos]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="14ex" flex-grow="1">
					<template class="header">
						Charging Key
					</template>
					<template>[[item.chargingKey]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						Service Id
					</template>
					<template>[[item.serviceId]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column-group>
					<template class="header">
						<div class="grouptitle">Service Flow</div>
					</template>
					<vaadin-grid-column width="25ex" flex-grow="3">
						<template class="header">
							Flow Up
						</template>
						<template>[[item.flowUp]]</template>
					</vaadin-grid-column>
					<vaadin-grid-column width="25ex" flex-grow="3">
						<template class="header">
							Flow Down
						</template>
						<template>[[item.flowDown]]</template>
					</vaadin-grid-column>
				</vaadin-grid-column-group>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						Precedence
					</template>
					<template>[[item.precedence]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column width="12ex" flex-grow="1">
					<template class="header">
						Predefined
					</template>
					<template>[[item.predefined]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<paper-dialog
					class="dialog"
					id="tableList">
				<app-toolbar>
					List of Tables
				</app-toolbar>
				<template
						id="tableItems"
						is="dom-repeat"
						items="[[tables]]">
					<paper-item
							class="menuitem"
							on-focused-changed="_tableSelection">
						<iron-icon icon ="icons:view-list" item-icon></iron-icon>
							{{item.name}}
					</paper-item>
				</template>
				<div class="buttons">
					<paper-button
							raised
							id="tabOkButton"
							disabled
							on-tap="_tableOk"
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
							on-tap="_tableAdd"
							class="submit-button">
						Add
					</paper-button>
					<paper-button
							raised
							on-tap="_tableDelete"
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
			<iron-ajax
					id="getPolicyRows">
			</iron-ajax>
			<iron-ajax
					id="getPolicyTables"
					url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=3"
					on-response="_getPolicyTablesResponse"
					rejectWithRequest>
			</iron-ajax>
			<iron-ajax
					id="deletePolicyTable"
					on-response="_deleteTableResponse"
					method="DELETE">
			</iron-ajax>
			<iron-ajax
					id="deletePolicyRow"
					method="DELETE">
			</iron-ajax>
			<iron-ajax
					id="policyUpdate"
					method="PATCH"
					loading="{{loading}}">
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
			activeTableName: {
				type: String,
				notify: true
			},
			activeTableId: {
				type: String
			},
			storeItem: {
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
		var ajax = this.shadowRoot.getElementById('getPolicyTables');
		ajax.generateRequest();
		this.$.tableList.open();
	}

	connectedCallback() {
		super.connectedCallback();
		this.addEventListener('iron-resize', this.onIronResize);
	}

	disconnectedCallback() {
		super.disconnectedCallback();
		this.removeEventListener('iron-resize', this.onIronResize);
	}

	onIronResize(event) {
		if (event.path[0].localName == 'iron-pages') {
			this.shadowRoot.getElementById('policyGrid').notifyResize();
		}
	}

	_tableOk() {
		document.body.querySelector('sig-app').viewTitle = 'Policy: ' + this.activeTableName;
		var grid = this.shadowRoot.getElementById('policyGrid');
		grid.dataProvider = this._getPolicy;
		grid.clearCache();
		this.$.tableList.close();
	} 

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.policyGrid;
			var current;
			if(item == null) {
				current = last;
			} else {
				current = item;
			}
			this.polId = current.id;
			this.polName = current.name;
			if(current.predefined) {
				this.predefined = true;
			} else {
				this.predefined = false;
			}
			this.polPrec = current.precedence;
			this.polCharKey = current.chargingKey;
			this.polcharSer = current.serviceId;
			this.widthDL = current.maxRequestedBandwidthDL;
			this.widthUL = current.maxRequestedBandwidthUL;
			this.classId = current.qosClassIdentifier;
			this.storeItem.push(current);
			function checkExist(policy) {
				return policy.id == current.id;
			}
			if(grid.detailsOpenedItems && grid.detailsOpenedItems.some(checkExist)) {
				grid.closeItemDetails(current);
			} else {
				grid.openItemDetails(current);
			}
		}
	}

	_getPolicyTablesResponse(event) {
		var results = event.detail.xhr.response;
		this.splice("tables", 0, this.tables.length)
		for (var indexTable in results) {
			var tableRecord = new Object();
			tableRecord.name = results[indexTable].name;
			tableRecord.id = results[indexTable].id;
			tableRecord.href = results[indexTable].href;
			tableRecord.flowDescription = results[indexTable].flowDescription;
			this.push('tables', tableRecord);
		}
		this.shadowRoot.getElementById('tableItems').notifyPath('items');
	}

	_flowPlus(event) {
		var domVar = this.shadowRoot.getElementById('flowDomRepeat');
		var flArr = event.model.item.flow;
		var flJson = {"flowDirection": "", "flowDescription": ""};
		this.push('activeItem.flow', flJson);
		domVar.notifyPath('items');
	}

	_flowMinus(event) {
		var policyDom = this.shadowRoot.getElementById('flowDomRepeat');
		var itemIndex = event.model.index;
		var itemArray = this.storeItem[0].flow;
		itemArray.splice(itemIndex, 1);
		policyDom.notifyPath('items');
	}

	_tableSelection(e) {
		if(e.model.item && e.model.item.id) {
			this.activeTableName = e.model.item.name;
			this.activeTableId = e.model.item.id;
			this.$.tabOkButton.disabled = false;
		} else {
			this.activeTableName = null;
			this.activeTableId = null;
			this.$.tabOkButton.disabled = true;
		}
	}

	_tableDelete() {
		this.$.deletePolicyTable.url = "/resourceInventoryManagement/v1/resource/" + this.activeTableId;
		this.$.deletePolicyTable.generateRequest();
		this.activeTableName = null;
		this.activeTableId = null;
	}

	_deleteTableResponse(event) {
		this.shadowRoot.getElementById('getPolicyTables').generateRequest();
	}

	_delete(event) {
		this.$.deletePolicyRow.url = "/resourceInventoryManagement/v1/resource/" + event.model.item.id;
		this.$.deletePolicyRow.generateRequest();
		this.shadowRoot.getElementById('policyGrid').clearCache();
	}

	_getPolicy(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var policyList = document.body.querySelector('sig-app').shadowRoot.getElementById('policyList');
		var ajax = policyList.shadowRoot.getElementById('getPolicyRows');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=4&resourceRelationship.resource.name=" + policyList.activeTableName;
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
					tabObj.flow = new Array();
					var resChar = request.response[index].resourceCharacteristic;
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "name") {
							tabObj.name = resChar[indexRes].value;
						} else if(resChar[indexRes].name == "predefined") {
							tabObj.predefined = resChar[indexRes].value;
						} else if(resChar[indexRes].name == "qosInformation") {
							var qos1 = resChar[indexRes].value.maxRequestedBandwidthDL;
							var qos2 = resChar[indexRes].value.maxRequestedBandwidthUL;
							var qos3 = resChar[indexRes].value.qosClassIdentifier;
							tabObj.maxRequestedBandwidthDL = qos1;
							tabObj.maxRequestedBandwidthUL = qos2;
							tabObj.qosClassIdentifier = qos3;
							tabObj.qos = "class:" + qos3 + ", UL:" + qos2 + ", DL:" + qos1;
						} else if(resChar[indexRes].name == "chargingKey") {
							tabObj.chargingKey = resChar[indexRes].value;
						} else if(resChar[indexRes].name == "flowInformation") {
							for(var indexFl in resChar[indexRes].value){
								if(resChar[indexRes].value[indexFl].flowDirection == "up") {
									var flowDirObj = new Object();
									flowDirObj.flowDirection = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj.flowDescription = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flow[indexFl] = flowDirObj;
									if(tabObj.flowUp) {
										tabObj.flowUp.concat("," + resChar[indexRes].value[indexFl].flowDescription);
									} else {
										tabObj.flowUp = resChar[indexRes].value[indexFl].flowDescription;
									}
								} else if(resChar[indexRes].value[indexFl].flowDirection == "down"){
									var flowDirObj1 = new Object();
									flowDirObj1.flowDirection = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj1.flowDescription = resChar[indexRes].value[indexFl].flowDescription;
									if(tabObj.flowDown) {
										tabObj.flowDown.concat("," + resChar[indexRes].value[indexFl].flowDescription);
									} else {
										tabObj.flowDown = resChar[indexRes].value[indexFl].flowDescription;
									}
								} else if(resChar[indexRes].value[indexFl].flowDirection == "both"){
									var flowDirObj1 = new Object();
									flowDirObj1.flowDirection = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj1.flowDescription = resChar[indexRes].value[indexFl].flowDescription;
									if(tabObj.flowUp) {
										tabObj.flowUp.concat("," + resChar[indexRes].value[indexFl].flowDescription);
									} else {
										tabObj.flowUp = resChar[indexRes].value[indexFl].flowDescription;
									}
									if(tabObj.flowDown) {
										tabObj.flowDown.concat("," + resChar[indexRes].value[indexFl].flowDescription);
									} else {
										tabObj.flowDown = resChar[indexRes].value[indexFl].flowDescription;
									}
								}
								tabObj.flowValueLength = tabObj.flow.length;
							}
						} else if(resChar[indexRes].name == "precedence") {
							tabObj.precedence = resChar[indexRes].value;
						} else if(resChar[indexRes].name == "serviceId") {
							tabObj.serviceId = resChar[indexRes].value;
						} else if(request.response[index].resourceRelationship) {
							tabObj.resourceRelationship = request.response[index].resourceRelationship;
						} else if(request.response[index].resourceSpecification) {
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
		}
		var handleAjaxError = function(error) {
			policyList.etag = null;
			var toast = policyList.shadowRoot.getElementById('PolicyToast');
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

	_tableAdd() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-table-add').shadowRoot.getElementById('addPolicyTableModal').open();
	}

	showAddPolicyModal(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-add').shadowRoot.getElementById('policyAddModal').open();
	}

	_update(event) {
		var getAjax = this.$.getPolicyRows;
		var etag = getAjax.lastRequest.xhr.getResponseHeader('ETag');
		var Mitem = event.model.item
		var results = getAjax.lastResponse;
		function checkId(charPolId) {
			return charPolId.id = Mitem.id; 
		}
		var index1 = results.findIndex(checkId);

		var updateAjax = this.$.policyUpdate;
		updateAjax.contentType = "application/json-patch+json";
		updateAjax.url = "/resourceInventoryManagement/v1/resource/" + Mitem.id;
		var PolArray = new Array();
		var indexChar = "-";
		if(Mitem.name) {
			var Name = new Object();
			Name.op = "add";
			Name.path = "/name";
			Name.value = Mitem.name;
			PolArray.push(Name);

			function checkName(charPol) {
				return charPol.name == "name";
			}
			var index2 = results[index1].resourceCharacteristic.findIndex(checkName);
			var Na = new Object();
			Na.op = "add";
			Na.path = "/resourceCharacteristic/" + index2 + "/value";
			Na.name = "name";
			Na.value = Mitem.name; 
			PolArray.push(Na);
		}
		if(Mitem.precedence) {
			function checkPrec(charPolPre) {
				return charPolPre.name == "precedence";
			}
			var index3 = results[index1].resourceCharacteristic.findIndex(checkPrec);
			var Pre = new Object();
			Pre.op = "add";
			Pre.path = "/resourceCharacteristic/" + index3 + "/value";
			Pre.name = "precedence";
			Pre.value = parseInt(Mitem.precedence);
			PolArray.push(Pre);
		}
		if(Mitem.chargingKey) {
			function checkChar(charPolCha) {
				return charPolCha.name == "chargingKey";
			}
			var index4 = results[index1].resourceCharacteristic.findIndex(checkChar);
			var Cha = new Object();
			Cha.op = "add";
			Cha.path = "/resourceCharacteristic/" + index4 + "/value";
			Cha.name = "chargingKey";
			Cha.value = parseInt(Mitem.chargingKey); 
			PolArray.push(Cha);
		}
		if(Mitem.maxRequestedBandwidthDL) {
			function checkQos(charPolQ) {
				return charPolQ.name == "qosInformation";
			}
			var index5 = results[index1].resourceCharacteristic.findIndex(checkQos);
			var Dl = new Object();
			Dl.op = "add";
			Dl.path = "/resourceCharacteristic/" + index5 + "/value/maxRequestedBandwidthDL";
			Dl.name = "qosInformation";
			Dl.value = parseInt(Mitem.maxRequestedBandwidthDL); 
			PolArray.push(Dl);
		}
		if(Mitem.maxRequestedBandwidthUL){
			function checkQos1(charPolQ1) {
				return charPolQ1.name == "qosInformation";
			}
			var index6 = results[index1].resourceCharacteristic.findIndex(checkQos1);
			var Ul = new Object();
			Ul.op = "add";
			Ul.path = "/resourceCharacteristic/" + index6 + "/value/maxRequestedBandwidthUL";
			Ul.name = "qosInformation";
			Ul.value = parseInt(Mitem.maxRequestedBandwidthUL); 
			PolArray.push(Ul);
		}
		if(Mitem.qosClassIdentifier) {
			function checkQos2(charPolQ2) {
				return charPolQ2.name == "qosInformation";
			}
			var index7 = results[index1].resourceCharacteristic.findIndex(checkQos2);
			var Cid = new Object();
			Cid.op = "add";
			Cid.path = "/resourceCharacteristic/" + index7 + "/value/qosClassIdentifier";
			Cid.name = "qosInformation";
			Cid.value = parseInt(Mitem.qosClassIdentifier); 
			PolArray.push(Cid);
		}

		if(Mitem.flow.length < Mitem.flowValueLength) {
			function checkFlow1(charPolFl1) {
				return charPolFl1.name == "flowInformation";
			}
			var indexFl = results[index1].resourceCharacteristic.findIndex(checkFlow1);
			var PolArray1 = new Array();
			var Flo1 = new Object();
			Flo1.op = "replace";
			Flo1.path = "/resourceCharacteristic/" + indexFl + "/value";
			Flo1.value = Mitem.flow;
			PolArray.push(Flo1);
		}

		if(Mitem.flow.length > Mitem.flowValueLength) {
			function checkFlow1(charPolFl1) {
				return charPolFl1.name == "flowInformation";
			}
			var indexFl = results[index1].resourceCharacteristic.findIndex(checkFlow1);
			var PolArray1 = new Array();
			var Flo1 = new Object();
			Flo1.op = "add";
			Flo1.path = "/resourceCharacteristic/" + indexFl + "/value/" + "-";
			Flo1.value = Mitem.flow;
			PolArray.push(Flo1);
		}

		if(Mitem.flow) {
			function checkFlow(charPolFl) {
				return charPolFl.name == "flowInformation";
			}
			var index8 = results[index1].resourceCharacteristic.findIndex(checkFlow);
			var arrayLength = Mitem.flow.length;
			for(var index8i = 0; index8i < arrayLength; index8i++) {
				var Fdir = new Object();
				Fdir.op = "replace";
				Fdir.path = "/resourceCharacteristic/" + index8 + "/value/" + index8i;
				Fdir.value = {"flowDirection": Mitem.flow[index8i].flowDirection, "flowDescription": Mitem.flow[index8i].flowDescription};
				PolArray.push(Fdir);
			}
		}

		updateAjax.body = JSON.stringify(PolArray);
		updateAjax.generateRequest();
		var policyList = document.body.querySelector('sig-app').shadowRoot.getElementById('policyList');
		var ajax = policyList.shadowRoot.getElementById('getPolicyRows');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=4&resourceRelationship.resource.name=" + policyList.table;
		ajax.generateRequest();
	}
}

window.customElements.define('sig-policy-list', policyList);
