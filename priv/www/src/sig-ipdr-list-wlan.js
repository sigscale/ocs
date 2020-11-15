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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-dialog-scrollable/paper-dialog-scrollable.js';
import '@polymer/iron-list/iron-list.js';
import '@polymer/iron-icons/iron-icons.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-toast/paper-toast.js';
import './style-element.js'

class ipdrListWlan extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="ipdrGrid"
					loading="{{loading}}"
					theme="no-border">
				<vaadin-grid-column
						width="19ex"
						flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="creationTime"
								path="ipdrCreationTime"
								value="{{_filterIPDRCreationTime}}">
							<input
									placeholder="Creation Time"
									value="{{_filterIPDRCreationTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.ipdrCreationTime]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="6ex">
					<template class="header">seq</template>
					<template>[[item.seqNum]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="18ex"
						flex-grow="3">
					<template class="header">
						<vaadin-grid-filter
								aria-label="userName"
								path="username"
								value="{{_filterUserName}}">
							<input
									placeholder="User Name"
									value="{{_filterUserName::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.username]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column 
						width="12ex"
						flex-grow="0">
					<template class="header">
						<vaadin-grid-filter
								aria-label="acctSessionId"
								path="acctSessionId"
								value="{{_filterAcctSessionId}}">
							<input
									placeholder="Session Id"
									value="{{_filterAcctSessionId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.acctSessionId]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="20ex"
						flex-grow="3">
					<template class="header">
						<vaadin-grid-filter
								aria-label="callingStationId"
								path="callingStationId"
								value="{{_filterCallingStationId}}">
							<input
									placeholder="Calling Station Id"
									value="{{_filterCallingStationId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.callingStationId]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="28ex"
						flex-grow="3">
					<template class="header">
						<vaadin-grid-filter
								aria-label="calledStationId"
								path="calledStationId"
								value="{{_filterCalledStationId}}">
							<input
									placeholder="Called Station Id"
									value="{{_filterCalledStationId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.calledStationId]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="16ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="nasIpAddress"
								path="nasIpAddress"
								value="{{_filterNasIpAddress}}">
							<input
									placeholder="NAS Address"
									value="{{_filterNasIpAddress::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.nasIpAddress]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="12ex"
						flex-grow="4">
					<template class="header">
						<vaadin-grid-filter
								aria-label="nasID"
								path="nasId"
								value="{{_filterNasId}}">
							<input
									placeholder="NAS ID"
									value="{{_filterNasId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.nasId]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="9ex"
						flex-grow="2">
					<template class="header">
						Duration
					</template>
					<template>[[item.sessionDuration]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="12ex"
						flex-grow="2">
					<template class="header">
						Input Octets
					</template>
					<template>[[item.inputOctets]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="13ex"
						flex-grow="2">
					<template class="header">
						Output Octets
					</template>
					<template>[[item.outputOctets]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="19ex"
						flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="gmtSessionStartDateTime"
								path="gmtSessionStartDateTime"
								value="{{_filterGmtSessionStartDateTime}}">
							<input
									placeholder="Start Time"
									value="{{_filterGmtSessionStartDateTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.gmtSessionStartDateTime]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="19ex"
						flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="gmtSessionendtime"
								path="gmtSessionEndDateTime"
								value="{{_filterGmtSessionEndDateTime}}">
							<input
									placeholder="End Time"
									value="{{_filterGmtSessionEndDateTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.gmtSessionEndDateTime]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="5ex">
					<template class="header">
						<vaadin-grid-filter
								aria-label="SessionTerminateCause"
								path="sessionTerminateCause"
								value="{{_filterSessionTerminateCause}}">
							<input
									Placeholder="Cause"
									value="{{_filterSessionTerminateCause::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.sessionTerminateCause]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<paper-dialog id="selectLogFileModalWlan">
				<app-toolbar>
					<h2>Select Log File</h2>
				</app-toolbar>
				<paper-dialog-scrollable>
					<iron-list id="logFilesWlan"
							as="item">
						<template>
							<div class="item" on-tap="getLogContentWlan">
								<iron-icon icon="assignment"></iron-icon>
										[[item]]
							</div>
						</template>
					</iron-list>
				</paper-dialog-scrollable>
				<div class="cancel-button">
					<paper-button dialog-dismiss>
							Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax id="getLogsAjaxWlan"
					method = "GET"
					headers='{"Accept": "application/json"}'
					on-response="getLogsResponseWlan"
					on-error="getLogsErrorWlan">
			</iron-ajax>
			<iron-ajax id="getIpdr"
					rejectWithRequest>
			</iron-ajax>
			<paper-toast id="getIpdrWlanToast">
			</paper-toast>
		`;
	}

	static get properties() {
		return {
			etag: {
				type: String,
				value: null
			},
			loading: {
				type: Boolean,
				notify: true
			},
			activePage: {
				type: Boolean,
				value: false,
				observer: '_activePageChanged'
			},
			_filterIPDRCreationTime: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterUserName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterAcctSessionId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCallingStationId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterCalledStationId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterNasIpAddress: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterNasId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterGmtSessionStartDateTime: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterGmtSessionEndDateTime: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSessionTerminateCause: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	refreshIpdr() {
		this.etag = null;
		delete this.$.getIpdr.params['date'];
		this.$.getLogsAjaxWlan.generateRequest();
		document.body.querySelector('sig-app').shadowRoot.getElementById('ipdrLogListWlan').shadowRoot.getElementById('ipdrGrid').clearCache();
	}

	_activePageChanged(active) {
		var wlanAjax = this.$.getLogsAjaxWlan;
		wlanAjax.url = "/ocs/v1/log/ipdr/wlan";
		wlanAjax.generateRequest();
		this.$.selectLogFileModalWlan.open();
	}

	intializeGrid(event) {
		var grid = this.shadowRoot.getElementById('ipdrGrid');
		grid.size = 0;
		var getIpdrWlan = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-wlan');
		var ajax = getIpdrWlan.shadowRoot.getElementById('getIpdr');
		ajax.url = "/usageManagement/v1/usage/ipdr/wlan/" + event.model.item;
		this.$.selectLogFileModalWlan.open();
		grid.dataProvider = this.getLogContentResponse;
	}

	getLogContentWlan(event) {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-wlan').shadowRoot.getElementById("ipdrListWlan").intializeGrid(event);
	}

	getLogsResponseWlan(event){
		this.$.logFilesWlan.items = event.detail.response;
	}

	getLogContentResponse(params, callback) {
		var grid = this;
		var getIpdrWlan1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-wlan');
		var ajax = getIpdrWlan1.shadowRoot.getElementById('getIpdr');
		ipdrLogListWlan = document.body.querySelector('sig-app').shadowRoot.querySelector('ipdrLogListWlan');
		handleAjaxResponse = function(request) {
			if (request) {
				ipdrLogListWlan.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range1[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range2[1]) + grid.pageSize * 2;
				}
				var vaadinItems = new Array();
				for(var index in request.response) {
					var newRecord = new Object();
					newRecord.date = request.response[index].date;
					newRecord.type = request.response[index].type;
					newRecord.usageSpecificationName = request.response[index].usageSpecification.name;
					request.response[index].usageCharacteristic.forEach(
						function(attrObj) {
							if(attrObj.value == "undefined") {
								attrObj.value = '';
								newRecord[attrObj.name] = attrObj.value;
							} else {
								newRecord[attrObj.name] = attrObj.value;
							}
						}
					);
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		}
		handleAjaxError = function(error) {
			ipdrLogListWlan.etag = null;
         this.$.getIpdrWlanToast.text = "Error";
         this.$.getIpdrWlanToast.open();
			callback([]);
		}
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (ipdrLogListWlan.etag && params.page > 0) {
					ajax.headers['If-Range'] = ipdrLogListWlan.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (ipdrLogListWlan.etag && params.page > 0) {
				ajax.headers['If-Range'] = ipdrLogListWlan.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('ipdrGrid');
		grid.size = 0;
	}
}

window.customElements.define('sig-ipdr-list-wlan', ipdrListWlan);
