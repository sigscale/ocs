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
							QOS Information
						</paper-tab>
						<paper-tab>
							Flow Information
						</paper-tab>
					</paper-tabs>
					<iron-pages
							selected="{{selectedTab}}">
						<div>
							<paper-input
									name="id"
									label="Id"
									value="{{polId}}"
									disabled>
							</paper-input>
							<paper-input
									name="name"
									label="Name"
									value="{{polName}}">
							</paper-input>
							<paper-input
									name="predefined"
									label="Predefined"
									value="{{predefined}}">
							</paper-input>
							<paper-input
									name="precedence"
									label="Precedence"
									value="{{item.precedence}}">
							</paper-input>
							<paper-input
									name="chargingKey"
									label="Charging Key"
									value="{{item.chargingKey}}">
							</paper-input>
							<paper-input
									name="serviceId"
									label="Service Id"
									value="{{item.serviceId}}">
							</paper-input>
						</div>
						<div>
							<paper-input
									name="maxRequestedBandwidthDL"
									label="Max Requested Bandwidth DL"
									value="{{item.maxRequestedBandwidthDL}}">
							</paper-input>
							<paper-input
									name="maxRequestedBandwidthUL"
									label="Max Requested Bandwidth UL"
									value="{{item.maxRequestedBandwidthUL}}">
							</paper-input>
							<paper-input
									name="qosClassIdentifier"
									label="QOS Class Identifier"
									value="{{item.qosClassIdentifier}}">
							</paper-input>
						</div>
						<div>
							<template id="flowDomRepeat" is="dom-repeat" items="{{item.flow}}" as="flowitem" mutable-data="true">
								<div class="flowRow">
									<paper-icon-button
											id$="min-[[item.id]]"
											class="minus-icon-button"
											icon="icons:remove-circle-outline"
											on-tap = "_flowMinus">
									</paper-icon-button>
									<paper-dropdown-menu
											id$="dir-[[item.id]]"
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
										</paper-listbox>
									</paper-dropdown-menu>
									<paper-input
											id$="desc-[[item.id]]"
											name="flowDescription"
											label="Flow Description"
											value="{{flowitem.flowDescription}}">
									</paper-input>
								<div>
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
				<vaadin-grid-column>
					<template class="header">
						Predefined
					</template>
					<template>[[item.predefined]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<paper-dialog class="dialog" id="tableList">
				<app-toolbar>
					List of Tables
				</app-toolbar>
				<template is="dom-repeat" items="[[tables]]">
					<paper-item
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
			<iron-ajax id="getPolicyContentAjax">
			</iron-ajax>
			<iron-ajax id="getPolicyAjax"
				url="/resourceInventoryManagement/v1/resource?resourceSpecification.id=3"
				on-response="_getPolicyResponse"
				rejectWithRequest>
			</iron-ajax>
			<iron-ajax id="deleteTableAjax">
			</iron-ajax>
			<iron-ajax id="deleteTableContentAjax">
			</iron-ajax>
			<iron-ajax
					id="policyUpdateAjax"
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

	tableOk() {
		var grid = this.shadowRoot.getElementById('policyGrid');
		grid.dataProvider = this._getPolicy;
		this.$.tableList.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	} 

	_activeItemChanged(item, last) {
		if(item || last) {
			var grid = this.$.policyGrid;
			var current;
			if(item == null) {
				current = last;
				grid.selectedItems = item ? [item] : [];
			} else {
				current = item;
				grid.selectedItems = [];
				this.polId = item.id;
				this.polName = item.name;
				if(item.predefined) {
					this.predefined = true;
				} else {
					this.predefined = false;
				}
				this.polPrec = item.precedence;
				this.polCharKey = item.chargingKey;
				this.polcharSer = item.serviceId;
				this.widthDL = item.maxRequestedBandwidthDL;
				this.widthUL = item.maxRequestedBandwidthUL;
				this.classId = item.qosClassIdentifier;
			}
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

	_getPolicyResponse(event) {
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
	}

	_flowPlus(event) {
		var domVar = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').shadowRoot.getElementById('flowDomRepeat')
		var flArr = event.model.item.flow;
		var flJson = {"flowDirection": flArr[0].flowDirection, "flowDescription": flArr[0].flowDescription};
		this.push('activeItem.flow', flJson);
		domVar.notifyPath('items');
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

	tableDelete() {
		var policyList1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
		this.$.deleteTableAjax.method = "DELETE";
		this.$.deleteTableAjax.url = "/resourceInventoryManagement/v1/resource/" + policyList1.tableId
		this.$.deleteTableAjax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('tableList').close();;
	}

	_delete(event) {
		this.$.deleteTableContentAjax.method = "DELETE";
		this.$.deleteTableContentAjax.url = "/resourceInventoryManagement/v1/resource/" + event.model.item.id;
		this.$.deleteTableContentAjax.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

	_getPolicy(params, callback) {
		var grid = this;
		if(!grid.size) {
			grid.size = 0;
		}
		var policyList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
		var ajax = policyList.shadowRoot.getElementById('getPolicyContentAjax');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=4&resourceRelationship.resource.name=" + policyList.table;
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
							tabObj.flow = new Array();
							for(var indexFl in resChar[indexRes].value){
								if(resChar[indexRes].value[indexFl].flowDirection == "up") {
									var flowDirObj = new Object();
									flowDirObj.flowDirection = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj.flowDescription = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flow[indexFl] = flowDirObj;
									var flowUpDirObj = resChar[indexRes].value[indexFl].flowDirection;
									var flowUpDesObj = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flowUp = flowUpDirObj + "," + flowUpDesObj;
								} else if(resChar[indexRes].value[indexFl].flowDirection == "down"){
									var flowDirObj1 = new Object();
									flowDirObj1.flowDirection = resChar[indexRes].value[indexFl].flowDirection;
									flowDirObj1.flowDescription = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flow[indexFl] = flowDirObj1;
									var flowDownDirObj = resChar[indexRes].value[indexFl].flowDirection;
									var flowDownDesObj = resChar[indexRes].value[indexFl].flowDescription;
									tabObj.flowDown = flowDownDirObj + "," + flowDownDesObj;
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

      if(Mitem.flow.length != Mitem.flowValueLength) {
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

		Ajax.body = JSON.stringify(PolArray);
		Ajax.generateRequest();
		var policyList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list');
		var ajax = policyList.shadowRoot.getElementById('getPolicyContentAjax');
		ajax.url = "/resourceInventoryManagement/v1/resource?resourceSpecification.id=4&resourceRelationship.resource.name=" + policyList.table;
		ajax.generateRequest();
	}
}

window.customElements.define('sig-policy-list', policyList);
