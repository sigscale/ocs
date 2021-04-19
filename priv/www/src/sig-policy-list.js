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
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
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
							QOS Information
						</paper-tab>
						<paper-tab>
							Flow Information
						</paper-tab>
					</paper-tabs>
					<iron-pages
							selected="{{selectedTab}}">
						<div>
							<dl class="details">
								<template is="dom-if" if="{{item.id}}">
									<paper-input
										id="addPolId"
										name="id"
										label="Id"
										value="{{polId}}"
										disabled>
									</paper-input>
								</template>
								<template is="dom-if" if="{{item.name}}">
									<paper-input
										id="addPolName"
										name="name"
										label="Name"
										value="{{polName}}">
									</paper-input>
								</template>
								<template is="dom-if" if="{{item.precedence}}">
									<paper-input
										id="addPolPre"
										name="precedence"
										label="Precedence"
										value="{{item.precedence}}">
									</paper-input>
								</template>
								<template is="dom-if" if="{{item.chargingKey}}">
									<paper-input
										id="addPolCha"
										name="chargingKey"
										label="Charging Key"
										value="{{item.chargingKey}}">
									</paper-input>
								</template>
								<template is="dom-if" if="{{item.serviceId}}">
									<paper-input
										id="addPolSer"
										name="serviceId"
										label="Service Id"
										value="{{item.serviceId}}">
									</paper-input>
								</template>
							</dl>
						</div>
						<div>
							<template is="dom-if" if="{{item.maxRequestedBandwidthDL}}">
										<paper-input
											id="addPolDL"
											name="maxRequestedBandwidthDL"
											label="Max Requested Bandwidth DL"
											value="{{item.maxRequestedBandwidthDL}}">
										</paper-input>
										<paper-input
											id="addPolUL"
											name="maxRequestedBandwidthUL"
											label="Max Requested Bandwidth UL"
											value="{{item.maxRequestedBandwidthUL}}">
										</paper-input>
										<paper-input
											id="addPolClass"
											name="qosClassIdentifier"
											label="QOS Class Identifier"
											value="{{item.qosClassIdentifier}}">
										</paper-input>
							</template>
						</div>
						<div>
							<template is="dom-if" if="{{item.flow}}">
								<table class="det">
									<template id="testMe" is="dom-repeat" items="{{item.flow}}" as="flowitem">
										<tr>
											<td><paper-dropdown-menu
													id="addPolDirDrop"
													value="{{flowitem.direction}}"
													no-animations="true"
													label="Flow Direction">
												<paper-listbox
														id="addPolDirList"
														slot="dropdown-content">
													<paper-item>
															up
													</paper-item>
													<paper-item>
															down
													</paper-item>
												</paper-listbox>
											</paper-dropdown-menu></td>
											<td><paper-input
													id="addPolDir"
													name="flowDescription"
													label="Flow Description"
													value="{{flowitem.description}}">
											</paper-input></td>
										</tr>
									</template>
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
									class="delete-button"
									on-tap="_delete">
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
					<template>[[item.chargingKey]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column>
					<template class="header">
						Service Id
					</template>
					<template>[[item.serviceId]]</template>
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
			<iron-ajax
					id="policyUpdateAjax"
					loading="{{loading}}"
					on-response="_response"
					on-error="_error">
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
				this.polId = item.id;
				this.polName = item.name;
				this.polPrec = item.precedence;
				this.polCharKey = item.chargingKey;
				this.polcharSer = item.serviceId;
				this.widthDL = item.maxRequestedBandwidthDL;
				this.widthUL = item.maxRequestedBandwidthUL;
				this.classId = item.qosClassIdentifier;
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
//					tabObj.resourceCharacteristic = request.response[index].resourceCharacteristic;
					var resChar = request.response[index].resourceCharacteristic;
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "name") {
							tabObj.name = resChar[indexRes].value;
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
							var flowUpDir = new Array();
							var flowUpDes = new Array();
							var flowDownDir = new Array();
							var flowDownDes = new Array();
							tabObj.flow = new Array();
							for(var indexFl in resChar[indexRes].value){
								if(resChar[indexRes].value[indexFl].flowDirection == "up") {
									var flowDirObj = new Object();
									flowDirObj.direction = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj.description = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flow[indexFl] = flowDirObj;
									var flowUpDirObj = resChar[indexRes].value[indexFl].flowDirection;
									var flowUpDesObj = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flowUp = flowUpDirObj + "," + flowUpDesObj;
								} else if(resChar[indexRes].value[indexFl].flowDirection == "down"){
									var flowDirObj1 = new Object();
									flowDirObj1.direction = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj1.description = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flow[indexFl] = flowDirObj1;
									var flowDownDirObj = resChar[indexRes].value[indexFl].flowDirection;
									var flowDownDesObj = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flowDown = flowDownDirObj + "," + flowDownDesObj;
								}
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

////////////////////////////////////////////////////////////////////////////
//Update Section
///////////////////////////////////////////////////////////////////////////

	_up(event) {
		var getAjax = this.$.getPolicyContentAjax;
		var etag = getAjax.lastRequest.xhr.getResponseHeader('ETag');
		var Mitem = event.model.item
		var results = getAjax.lastResponse;
		function checkId(charPolId) {
			return charPolId.id = Mitem.id; 
		}
		var index1 = results.findIndex(checkId);

		var Ajax = this.$.policyUpdateAjax;
		Ajax.method = "PATCH"
		Ajax.contentType = "application/json-patch+json";
		Ajax.url = "/resourceInventoryManagement/v1/resource/" + Mitem.id;
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
			Na.minCardinality = 1;
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
			Pre.minCardinality = 1;
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
			Cha.minCardinality = 0;
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
			Dl.minCardinality = 0;
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
			Ul.minCardinality = 0;
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
			Cid.minCardinality = 0;
			Cid.value = parseInt(Mitem.qosClassIdentifier); 
			PolArray.push(Cid);
		}
/*		if(Mitem.flow) {
			for (var indexFlow in Mitem.flow) {
				function checkFlow(charPolFl) {
					return charPolFl.name == "flowInformation";
				}
				var index8 = results[index1].resourceCharacteristic.findIndex(checkFlow);
				function checkFlow2(charPolFl2) {
					return charPolFl2.flowDirection == Mitem.flow[indexFlow].direction;
				}
				var index8i = results[index1].resourceCharacteristic[index8].value.findIndex(checkFlow2);
				var Fdir = new Object();
				Fdir.op = "add";
				Fdir.path = "/resourceCharacteristic/" + index8 + "/value/" + index8i + "/flowDirection";
				Fdir.name = "flowInformation";
				Fdir.minCardinality = 1;
				Fdir.value = Mitem.flow[indexFlow].direction;
				PolArray.push(Fdir);
			}
		}
		if(this.flowDescriptionV) {
			function checkFlow1(charPolFl1) {
				return charPolFl1.name == "flowInformation";
			}
			var index9 = results[index1].resourceCharacteristic.findIndex(checkFlow1);
			var floDes1 = this.flowDescriptionV1;
			function checkFlow2(charPolFl2) {
				return charPolFl2.flowDescription == floDes1;
			}
			var index10 = results[index1].resourceCharacteristic[index9].value.findIndex(checkFlow2);
			var Fdes = new Object();
			Fdes.op = "add";
			Fdes.path = "/resourceCharacteristic/" + index9 + "/value/" + index10 + "/flowDescription";
			Fdes.name = "flowInformation";
			Fdes.minCardinality = 1;
			Fdes.value = this.flowDescriptionV;
			PolArray.push(Fdes);
		}*/
		Ajax.body = JSON.stringify(PolArray);
		Ajax.generateRequest();
	}
}

window.customElements.define('sig-policy-list', policyList);
