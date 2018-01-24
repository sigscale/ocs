<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="vaadin-grid/vaadin-grid.html">
<link rel="import" href="paper-fab/paper-fab.html" >
<link rel="import" href="i18n-msg/i18n-msg.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="iron-ajax/iron-ajax.html">

<dom-module id="sig-prefix-list">
	<template>
		<style>
			.add-button {
				right: 2%;
				position: fixed;
				bottom: 5%;
				z-index: 100;
			}
			paper-fab {
				background: var(--paper-lime-a700);
				color: black;
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
		</style>
		<vaadin-grid id="prefixGrid" active-item="{{activeItem}}">
			<vaadin-grid-column width="15ex">
				<template class="header">
					<i18n-msg msgid="prefix">
						Prefix
					</i18n-msg>
				</template>
				<template>[[item.prefix]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="15ex">
				<template class="header">
					<i18n-msg msgid="des">
						Description	
					</i18n-msg>
				</template>
				<template>[[item.description]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="15ex">
				<template class="header">
					<i18n-msg msgid="rate">
						Rate
					</i18n-msg>
				</template>
				<template>[[item.rate]]</template>
			</vaadin-grid-column>
		</vaadin-grid>
		<div class="add-button">
			<paper-fab
					icon="add"
					on-tap="showAddPrefixModal">
			</paper-fab>
		</div>
		<iron-ajax id="getTableContentAjax"
			on-response="_getTableContentResponse"
			on-error="_getTableContentError">
		</iron-ajax>
	</template>
	<script>
		var cbPrefix;
		Polymer ({
			is: 'sig-prefix-list',
			behaviors: [i18nMsgBehavior],
			properties: {
				table: {
					type: String
				},
				activePage: {
					type: Boolean,
					value: false,
					observer: '_activePageChanged'
				},
				activeItem: {
					observer: '_activeItemChanged'
				}
			},
			_activePageChanged: function(active) {
				if (active) {
					var grid = this.$.prefixGrid;
					grid.columns = [
						{
							name: "prefix"
						},
						{
							name: "description"
						},
						{
							name: "rate"
						}
					];
					grid.dataProvider = function(params, callback) {
						cbPrefix = callback;
						var ajax = document.getElementById('getTableContentAjax');
						var table = document.getElementById('prefixList').table;
						ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + table;
						ajax.generateRequest();
					};
				}
			},
			_getTableContentResponse: function(event) {
				var grid = this.$.prefixGrid;
				var results = event.detail.xhr.response;
				var vaadinItems = new Array();
				for (var index in results) {
					var resChar = results[index].resourceCharacteristic;
					var tabObj = new Object();
					for (var indexRes in resChar) {
						if(resChar[indexRes].name == "prefix") {
							tabObj.prefix = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "description") {
							tabObj.description = resChar[indexRes].value.value;
						}
						if(resChar[indexRes].name == "rate") {
							tabObj.rate = resChar[indexRes].value.value;
						}
						vaadinItems[index] = tabObj;
					}
				}
				grid.size = vaadinItems.length;
				cbPrefix(vaadinItems);
			},
			_activeItemChanged: function(item) {
				if(item != null) {
					this.$.prefixGrid.selectedItems = item ? [item] : [];
					document.getElementById("updatePrefixModal").open();
					document.getElementById("updatePrefix").value = item.prefix;
					document.getElementById("updateDescription").value = item.description;
					document.getElementById("updateRate").value = item.rate;
				}
			},
			showAddPrefixModal: function() {
				document.getElementById("addPrefixModal").open();
			}
		});
	</script>
</dom-module>
