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
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import './style-element.js'

class ipdrListVoip extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="ipdrGridVoip"
					loading="{{loading}}">
				<vaadin-grid-column
						width="19ex"
						flex-grow="1">
					<template class="header">
						<vaadin-grid-filter
								aria-label="call completion"
								path="callCompletionCode"
								value="{{_filterCallCompletionCode}}">
							<input
									placeholder="call Completion Code"
									value="{{_filterCallCompletionCode::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.callCompletionCode]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="3">
					<template class="header">
						<vaadin-grid-filter
								aria-label="host name"
								path="hostName"
								value="{{_filterHostName}}">
							<input
									placeholder="Host Name"
									value="{{_filterHostName::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.hostName]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="3">
					<template class="header">
						<vaadin-grid-filter
								aria-label="subscriber id"
								path="subscriberId"
								value="{{_filterSubscriberId}}">
							<input
									placeholder="Subscriber Id"
									value="{{_filterSubscriberId::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.subscriberId]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="uniqueCallId"
								path="uniqueCallID"
								value="{{_filterUniqueCallID}}">
							<input
									placeholder="UniqueCall Id"
									value="{{_filterUniqueCallID::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.uniqueCallID]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="disconnect reason"
								path="disconnectReason"
								value="{{_filterDisconnectReason}}">
							<input
									placeholder="Disconnect Reason"
									value="{{_filterDisconnectReason:input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.disconnectReason]]
					</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="destination id"
								path="destinationID"
								value="{{_filterDestinationID}}">
							<input
									placeholder="Destination Id"
									value="{{_filterDestinationID:input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>
							[[item.destinationID]]
					</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax id="getIpdrVoip"
					rejectWithRequest>
			</iron-ajax>
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
			_filterCallCompletionCode: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterHostName: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterSubscriberId: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterUniqueCallID: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterDisconnectReason: {
				type: Boolean,
				observer: '_filterChanged'
			},
			_filterDestinationID: {
				type: Boolean,
				observer: '_filterChanged'
			}
		}
	}

	_activePageChanged(active) {
		if(active) {
			var voipAjax = document.getElementById("getLogsAjaxVoip");
			voipAjax.url = "/ocs/v1/log/ipdr/voip";
			voipAjax.generateRequest();
			document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-log-files-voip').shadowRoot.getElementById('selectLogFileModalVoip').open();
		}
	}

   intializeGrid(event) {
      super.ready();
      var grid = this.shadowRoot.getElementById('ipdrGridVoip');
		grid.size = 0;
		var getIpdr = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-voip');
		var ajax = getIpdr.shadowRoot.getElementById('getIpdrVoip');
		ajax.url = "/usageManagement/v1/usage/ipdr/voip/" + event.model.item;
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-log-files-voip').shadowRoot.getElementById('selectLogFileModalVoip').open();
      grid.dataProvider = this.getLogContentResponseVoip;
   }

	getLogContentResponseVoip(params, callback) {
		var grid = this;
		var getIpdr1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-ipdr-list-voip');
		var ajax = getIpdr1.shadowRoot.getElementById('getIpdrVoip');
		var ipdrLogListVoip = document.body.querySelector('sig-app').shadowRoot.querySelector('ipdrLogListVoip');
		var handleAjaxResponse = function(request) {
			if (request) {
				ipdrLogListVoip.etag = request.xhr.getResponseHeader('ETag');
				var range = request.xhr.getResponseHeader('Content-Range');
				var range1 = range.split("/");
				var range2 = range1[0].split("-");
				if (range[1] != "*") {
					grid.size = Number(range1[1]);
				} else {
					grid.size = Number(range[1]) + grid.pageSize * 2;
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
					});
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		}
		var handleAjaxError = function(error) {
			ipdrLogListVoip.etag = null;
			var toast = document.getElementById('usageToastErrorVoip');
			toast.text = error;
			toast.open();
			callback([]);
		}
		if (ajax.loading) {
			ajax.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				var endRange = startRange + params.pageSize - 1;
				ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (ipdrLogListVoip.etag && params.page > 0) {
					ajax.headers['If-Range'] = ipdrLogListVoip.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
			},handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (ipdrLogListVoip.etag && params.page > 0) {
				ajax.headers['If-Range'] = ipdrLogListVoip.etag;
			} else {
				delete ajax.headers['If-Range'];
			}
			ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}

	_filterChanged(filter) {
		this.etag = null;
		var grid = this.shadowRoot.getElementById('ipdrGridVoip');
		grid.size = 0;
	}
}

window.customElements.define('sig-ipdr-list-voip', ipdrListVoip);

/*	<script>
			refreshIPDRVoip: function() {
				this.etag = null;
				delete this.$.getIpdrVoip.params['date'];
				document.getElementById("getLogsAjaxVoip").generateRequest();
				document.getElementById("ipdrGridVoip").clearCache();
			},*/
