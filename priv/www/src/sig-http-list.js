<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="vaadin-grid/vaadin-grid.html">
<link rel="import" href="vaadin-grid/vaadin-grid-filter.html">
<link rel="import" href="iron-ajax/iron-ajax.html">

<dom-module id="sig-http-list">
	<template>
		<style>
			::-webkit-input-placeholder { /* Chrome/Opera/Safari */
				color: initial;
				font-weight: bold;
			}
			::-moz-placeholder { /* Firefox 19+ */
				color: initial;
				font-weight: bold;
			}
			:-ms-input-placeholder { /* IE 10+ */
				color: initial;
				font-weight: bold;
			}
			:-moz-placeholder { /* Firefox 18- */
				color: initial;
				font-weight: bold;
			}
			vaadin-grid col {
				font-size: 46px
			}
			vaadin-grid {
				height: 100%;
				--vaadin-grid-header-cell: {
					background: #ffb04c;
				};
			}
			vaadin-grid input {
				font-size: inherit;
				background: #ffb04c;
				border-style: none;
			}
			.yellow-button {
				text-transform: none;
				color: #eeff41;
			}
		</style>
		<vaadin-grid id="httpGrid">
			<vaadin-grid-column width="28ex" flex-grow="2">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.dateTime]]" path="datetime" value="[[_filterDateTime]]">
						<input placeholder="[[i18n.dateTime]]" value="{{_filterDateTime::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.datetime]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="15ex" flex-grow="4">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.host]]" path="host" value="[[_filterHost]]">
						<input placeholder="[[i18n.host]]" value="{{_filterHost::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.host]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="8ex" flex-grow="5">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.user]]" path="user" value="[[_filterUser]]">
						<input placeholder="[[i18n.user]]" value="{{_filterUser::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.user]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="8ex" flex-grow="2">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.method]]" path="method" value="[[_filterMethod]]">
						<input placeholder="[[i18n.method]]" value="{{_filterMethod::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.method]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="40ex" flex-grow="15">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.resource]]" path="uri" value="[[_filterResource]]">
						<input placeholder="[[i18n.resource]]" value="{{_filterResource::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.uri]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="7ex" flex-grow="0">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.status]]" path="httpStatus" value="[[_filterStatus]]">
						<input placeholder="[[i18n.status]]" value="{{_filterStatus::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.httpStatus]]</template>
			</vaadin-grid-column>
		</vaadin-grid>
		<paper-toast id="httpErrorToast" duration="0">
			<paper-button
					class="yellow-button"
					onclick="httpErrorToast.toggle()">
				Close
			</paper-button>
		</paper-toast>
		<iron-ajax id="getHttp"
			url="/ocs/v1/log/http"
			method = "GET"
			headers='{"Accept": "application/json"}'
			on-loading-changed="_onLoadingChanged"
			on-response="getHttpResponse"
			on-error="getHttpError">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-http-list',
			behaviors: [i18nMsgBehavior],
			properties: {
				activePage: {
					type: Boolean,
					value: false,
					observer: '_activePageChanged'
				}
			},
			_activePageChanged: function(active) {
				if (active) {
					this.$.getHttp.generateRequest();
				}
			},
			getHttpResponse: function(event) {
				var grid = this.$.httpGrid;
				var results = event.detail.xhr.response;
				var vaadinItems = new Array();
				for (var index in results) {
					var newRecord = new Object();
					newRecord.datetime = results[index].datetime;
					newRecord.host = results[index].host;
					newRecord.user = results[index].user;
					newRecord.method = results[index].method;
					newRecord.uri = results[index].uri;
					newRecord.httpStatus = results[index].httpStatus;
					vaadinItems[index] = newRecord;
				}
				grid.items = vaadinItems;
				grid.frozenColumns = 2;
				grid.columns = [
					{
						name: "datetime"
					},
					{
						name: "host"
					},
					{
						name: "user"
					},
					{
						name: "method"
					},
					{
						name: "uri"
					},
					{
						name: "httpStatus"
					}
				];
			},
			getHttpError: function (event) {
				this.$.httpErrorToast.text = event.detail.request.xhr.statusText;
				this.$.httpErrorToast.open();
			},
			_onLoadingChanged: function(event) {
				if (this.$.getHttp.loading) {
					document.getElementById("progress").disabled = false;
				} else {
					document.getElementById("progress").disabled = true;
				}
			}
		});
	</script>
</dom-module>
