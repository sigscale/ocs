<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="vaadin-grid/vaadin-grid.html">
<link rel="import" href="vaadin-grid/vaadin-grid-filter.html">
<link rel="import" href="vaadin-grid/vaadin-grid-column-group.html">
<link rel="import" href="iron-ajax/iron-ajax.html">

<dom-module id="sig-balance-list">
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
			vaadin-grid .grouptitle {
				text-align: center;
				border-bottom-style: solid;
				border-color: var(--paper-yellow-900);
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
		<vaadin-grid id="balanceGrid">
			<vaadin-grid-column width="24ex">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.timeStamp]]" path="timeStamp"
						value="[[filterTime]]">
						<input placeholder="[[i18n.timeStamp]]" value="{{filterTime::input}}"
							focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.timeStamp]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column id="check" width="10ex" flex-grow="2">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.type]]" path="type"
						value="[[filterType]]">
						<input placeholder="[[i18n.type]]" value="{{filterType::input}}"
							focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.type]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column id="check1"width="14ex">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.prod]]" path="product"
						value="[[filterProduct]]">
						<input placeholder="[[i18n.prod]]" value="{{filterProduct::input}}"
							focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.product]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column id="check2" width="12ex" flex-grow="2">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.bucket]]" path="bucket"
						value="[[filterBucket]]">
						<input placeholder="[[i18n.bucket]]" value="{{filterBucket::input}}"
							focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.bucket]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column id="check3" width="13ex" flex-grow="2">
				<template class="header">
					<vaadin-grid-filter aria-label="[[i18n.amountTransaction]]" path="amount"
						value="[[filterAmount]]">
						<input placeholder="[[i18n.amountTransaction]]" value="{{filterAmount::input}}"
							focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.amount]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column-group id="check4">
				<template class="header">
					<div class="grouptitle">Remain Amount</div>
				</template>
				<vaadin-grid-column id="check5" width="10ex" flex-grow="2">
					<template class="header">
						<vaadin-grid-filter aria-label="[[i18n.amountAfter]]" path="amountAfter"
							value="[[filterAfter]]">
							<input placeholder="[[i18n.amountAfter]]" value="{{filterAfter::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.amountAfter]]</template>
				</vaadin-grid-column>
				<vaadin-grid-column id="check6" width="12ex">
					<template class="header">
						<vaadin-grid-filter aria-label="[[i18n.amountBefore]]" path="amountBefore"
							value="[[filterBefore]]">
							<input placeholder="[[i18n.amountBefore]]" value="{{filterBefore::input}}"
								focus-target>
						</vaadin-grid-filter>
					</template>
					<template>[[item.amountBefore]]</template>
				</vaadin-grid-column>
			</vaadin-grid-column-group>
		</vaadin-grid>
		<iron-ajax id="getBalanceAjax"
			url="/ocs/v1/log/balance"
			rejectWithRequest>
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-balance-list',
			behaviors: [i18nMsgBehavior],
			properties: {
				activePage: {
					type: Boolean,
					value: false,
					observer: '_activePageChanged'
				},
				filterTime: {
					observer: '_filterChangedTime'
				},
				filterType: {
					observer: '_filterChangedType'
				},
				filterProduct: {
					observer: '_filterChangedProduct'
				},
				filterBucket: {
					observer: '_filterChangedBucket'
				},
				filterAmount: {
					observer: '_filterChangedAmount'
				},
				filterAfter: {
					observer: '_filterChangedAfter'
				},
				filterBefore: {
					observer: '_filterChangedBefore'
				}
			},
			_activePageChanged: function(active) {
				if(active) {
					var grid = this.$.balanceGrid;
					grid.columns = [
						{
							name: "timeStamp"
						},
						{
							name: "type"
						},
						{
							name: "product"
						},
						{
							name: "bucket"
						},
						{
							name: "amount"
						},
						{
							name: "amountBefore"
						},
						{
							name: "amountAfter"
						}
					];
					grid.dataProvider = this._getBalanceResponse;
				}
			},
			_filterChangedTime: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedType: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedProduct: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedBucket: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedAmount: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedAfter: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_filterChangedBefore: function(filter) {
				this.etag = null;
				this.$.balanceGrid.size = 0;
			},
			_getBalanceResponse: function(params, callback) {
				var grid = document.getElementById('balanceGrid');
				var ajax = document.getElementById('getBalanceAjax');
				delete ajax.params['filter'];
				function checkHead(param) {
					return param.path == "timeStamp" || param.path == "type" ||
					param.path == "amount" || param.path == "bucket" ||
					param.path == "amountBefore" || param.path == "amountAfter" ||
					param.path == "product";
				}
				params.filters.filter(checkHead).forEach(function(filter) {
					if (filter.value) {
						if(ajax.params['filter']) {
							ajax.params['filter'] += "]," + filter.path + ".like=[" + filter.value;
						} else {
							ajax.params['filter'] = "\"[{" + filter.path + ".like=[" + filter.value;
						}
					}
				});
				if (ajax.params['filter']) {
					ajax.params['filter'] += "]}]\"";
				}
				var balanceList = document.getElementById('balanceList');
				var handleAjaxResponse = function(request) {
				if (request) {
					grid.size = 100;
					var vaadinItems = new Array();
					var results = request.response;
					for (var index in results) {
						var balanceLog = new Object();
						balanceLog.timeStamp = results[index].date;
						balanceLog.type = results[index].type;
						balanceLog.amount = results[index].amount.amount;
						balanceLog.bucket = results[index].bucketBalance.id;
						balanceLog.amountBefore = results[index].amountBefore.amount;
						balanceLog.amountAfter = results[index].amountAfter.amount;
						balanceLog.product = results[index].product.id;
						vaadinItems[index] = balanceLog;
					}
					callback(vaadinItems);
				} else {
					grid.size = 0;
					callback([]);
				}
			};
			var handleAjaxError = function(event) {
				balanceList.etag = null;
				var toast = document.getElementById('usageToastError');
				this.$.balanceErrorToast.text = event.detail.request.xhr.statusText;
				this.$.balanceErrorToast.open();
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
				if (balanceList.etag && params.page > 0) {
					ajax.headers['If-Range'] = balanceList.etag;
				} else {
					delete ajax.headers['If-Range'];
				}
				return ajax.generateRequest().completes;
				}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
				} else {
					var startRange = params.page * params.pageSize + 1;
					var endRange = startRange + params.pageSize - 1;
					ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (balanceList.etag && params.page > 0) {
						ajax.headers['If-Range'] = balanceList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
				}
			}
		});
	</script>
</dom-module>
