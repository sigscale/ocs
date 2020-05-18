<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-item/paper-item.html">
<link rel="import" href="paper-item/paper-icon-item.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-input/paper-input.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="paper-tooltip/paper-tooltip.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="iron-icons/iron-icons.html">
<link rel="import" href="iron-icons/communication-icons.html">

<dom-module id="sig-prefix-update">
	<template>
		<style is="custom-style">
			paper-dialog {
				overflow: auto;
			}
			paper-input {
				--paper-input-container-focus-color: var(--paper-yellow-900);
			}
			paper-toolbar{
				margin-top: 0px;
				color: white;
				background-color: #bc5100;
			}
			.add-button {
				background-color: var(--paper-lime-a700);
				color: black;
				width: 8em;
			}
			.cancel-button {
				color: black;
			}
			.delete-buttons {
				background: #EF5350;
				color: black;
			}
		</style>
		<paper-dialog id="updatePrefixModal" modal>
			<paper-toolbar>
				<h2>[[i18n.updatePrefix]]</h2>
			</paper-toolbar>
			<div>
				<paper-input
					id="updatePrefix"
					name="Prefix"
					label="[[i18n.prefix]]"
					disabled>
				</paper-input>
			</div>
			<div>
				<paper-input
					id="updateDescription"
					name="Description"
					label="[[i18n.des]]">
				</paper-input>
			</div>
			<div>
				<paper-input
					id="updateRate"
					name="rateUpdate"
					label="[[i18n.rate]]"
					type="number">
				</paper-input>
			</div>
			<div class="buttons">
				<paper-button dialog-confirm
						raised
						class="add-button"
						on-tap="UpdateButton">
					<i18n-msg msgid="update">
							Update
					</i18n-msg>
				</paper-button>
				<paper-button dialog-dismiss
						class="cancel-button"
						dialog-dismiss
						on-tap="cancelUpdate">
					<i18n-msg msgid="cancel">
							Cancel
					</i18n-msg>
				</paper-button>
				<paper-button toggles
						raised
						on-tap="deleteUpdate"
						class="delete-buttons">
					<i18n-msg msgid="delete">
						Delete
					<i18n-msg>
				</paper-button>
			</div>
		</paper-dialog>
		<paper-toast
			id="updateTableRowToastError">
		</paper-toast>
		<paper-toast
			id="deleteTableRowToastError">
		</paper-toast>
		<iron-ajax id="updateTableRowAjax"
			on-response="_updateTableRowResponse"
			on-error="_updateTableRowError"
			on-loading-changed="_onLoadingChanged">
		</iron-ajax>
		<iron-ajax id="deleteTableRowAjax"
			on-response="_deleteTableRowResponse"
			on-error="_deleteTableRowError">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-prefix-update',
			behaviors: [i18nMsgBehavior],
			UpdateButton: function(event) {
				var Ajax = this.$.updateTableRowAjax;
				Ajax.method = "PATCH";
				Ajax.contentType = "application/json-patch+json";
				var Table = document.getElementById('prefixList').table;
				var Id = this.$.updatePrefix.value;
				Ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + Table + "/" + Id;
				var ResArray = new Array();
				var Desc = new Object(); 
				Desc.op = "add";
				Desc.path = "/resourceCharacteristic/1/value/value";
				Desc.value = this.$.updateDescription.value;
				ResArray.push(Desc);
				var Rate = new Object();
				Rate.op = "add";
				Rate.path = "/resourceCharacteristic/2/value/value";
				Rate.value = this.$.updateRate.value; 
				ResArray.push(Rate);
				Ajax.body = JSON.stringify(ResArray);
				Ajax.generateRequest();
				this.$.updatePrefix.value = null;
				this.$.updateDescription.value = null;
				this.$.updateRate.value = null;
			},
			_updateTableRowResponse: function(event) {
				this.$.updatePrefixModal.close();
				document.getElementById("prefixGrid").clearCache();
			},
			_updateTableRowError: function(event) {
				this.$.updateTableRowToastError.text = event.detail.request.xhr.statusText;
				this.$.updateTableRowToastError.open();
			},
			deleteUpdate: function(event) {
				var Table = document.getElementById('prefixList').table;
				var Id = this.$.updatePrefix.value;
				this.$.deleteTableRowAjax.method = "DELETE";
				this.$.deleteTableRowAjax.url = "/resourceInventoryManagement/v1/logicalResource/"
						+ Table + "/" + Id;
				this.$.deleteTableRowAjax.generateRequest();
			},
			_deleteTableRowResponse: function(event) {
				this.$.updatePrefixModal.close();
				document.getElementById("prefixGrid").clearCache();
			},
			_deleteTableRowError: function(event) {
				this.$.deleteTableRowToastError.text = event.detail.request.xhr.statusText;
				this.$.deleteTableRowToastError.open();
			},
			cancelUpdate: function() {
				this.$.updatePrefix.value = null;
				this.$.updateDescription.value = null;
				this.$.updateRate.value = null;
			},
			_onLoadingChanged: function(event) {
				if (this.$.updateTableRowAjax.loading) {
					document.getElementById("progress").disabled = false;
				} else {
					document.getElementById("progress").disabled = true;
				}
			}
		});
	</script>
</dom-module>
