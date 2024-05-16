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
import '@vaadin/vaadin-grid/theme/material/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid.js';
import '@vaadin/vaadin-grid/vaadin-grid-filter.js';
import '@vaadin/vaadin-grid/vaadin-grid-column-group.js';
import './style-element.js'

class httpList extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<vaadin-grid
					id="httpGrid"
					loading="{{loading}}"
					theme="no-border">
				<vaadin-grid-column
						width="28ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="dateTime"
								path="datetime"
								value="{{_filterDateTime}}">
							<input
									slot="filter"
									placeholder="Date & Time"
									value="{{_filterDateTime::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.datetime]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="15ex"
						flex-grow="4">
					<template class="header">
						<vaadin-grid-filter
								aria-label="host"
								path="host"
								value="{{_filterHost}}">
							<input
									slot="filter"
									placeholder="Host"
									value="{{_filterHost::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.host]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="8ex"
						flex-grow="5">
					<template class="header">
						<vaadin-grid-filter
								aria-label="user"
								path="user"
								value="{{_filterUser}}">
							<input
									slot="filter"
									placeholder="User"
									value="{{_filterUser::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.user]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="8ex"
						flex-grow="2">
					<template class="header">
						<vaadin-grid-filter
								aria-label="method"
								path="method"
								value="{{_filterMethod}}">
							<input
									slot="filter"
									placeholder="Method"
									value="{{_filterMethod::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.method]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="40ex"
						flex-grow="15">
					<template class="header">
						<vaadin-grid-filter
								aria-label="resource"
								path="uri"
								value="{{_filterResource}}">
							<input
									slot="filter"
									placeholder="Resource"
									value="{{_filterResource::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.uri]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column
						width="7ex"
						flex-grow="0">
					<template class="header">
						<vaadin-grid-filter
								aria-label="status"
								path="httpStatus"
								value="{{_filterStatus}}">
							<input
									slot="filter"
									placeholder="Status"
									value="{{_filterStatus::input}}"
									focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.httpStatus]]</template>
				</vaadin-grid-column>
			</vaadin-grid>
			<iron-ajax id="getHttp"
					url="/ocs/v1/log/http"
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
			activePage: {
				type: Boolean,
				value: false
			}
		}
	}

	ready() {
		super.ready();
		var grid = this.shadowRoot.getElementById('httpGrid');
		grid.dataProvider = this._getHttpResponse;
	}

	_getHttpResponse(params, callback) {
		var grid = this;
		var httpList = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-http-list').shadowRoot.getElementById('getHttp');
		var httpList1 = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-http-list');
		var handleAjaxResponse = function(request) {
			if(request) {
				httpList1.etag = request.xhr.getResponseHeader('ETag');
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
					newRecord.datetime = request.response[index].datetime;
					newRecord.host = request.response[index].host;
					newRecord.user = request.response[index].user;
					newRecord.method = request.response[index].method;
					newRecord.uri = request.response[index].uri;
					newRecord.httpStatus = request.response[index].httpStatus;
					vaadinItems[index] = newRecord;
				}
				callback(vaadinItems);
			} else {
				grid.size = 0;
				callback([]);
			}
		};
		var handleAjaxError = function(error) {
			httpList1.etag = null;
			var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
			toast.text = "Error";
			toast.open();
			if(!grid.size) {
				grid.size = 0;
			}
			callback([]);
		}
		if(httpList.loading) {
			httpList.lastRequest.completes.then(function(request) {
				var startRange = params.page * params.pageSize + 1;
				httpList.headers['Range'] = "items=" + startRange + "-" + endRange;
				if (httpList1.etag && params.page > 0) {
					httpList.headers['If-Range'] = httpList1.etag;
				} else {
					delete httpList.headers['If-Range'];
				}
				return httpList.generateRequest().completes;
			}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
		} else {
			var startRange = params.page * params.pageSize + 1;
			var endRange = startRange + params.pageSize - 1;
			httpList.headers['Range'] = "items=" + startRange + "-" + endRange;
			if (httpList1.etag && params.page > 0) {
				httpList.headers['If-Range'] = httpList1.etag;
			} else {
				delete httpList.headers['If-Range'];
			}
			httpList.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
		}
	}
}

window.customElements.define('sig-http-list', httpList);
