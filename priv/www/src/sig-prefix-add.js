<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-dropdown-menu/paper-dropdown-menu.html">
<link rel="import" href="paper-listbox/paper-listbox.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-input/paper-input.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="paper-tooltip/paper-tooltip.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="paper-menu-button/paper-menu-button.html" />
<link rel="import" href="paper-icon-button/paper-icon-button.html">
<link rel="import" href="iron-icon/iron-icon.html">
<link rel="import" href="iron-icons/iron-icons.html">

<dom-module id="sig-prefix-add">
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
		</style>
		<paper-dialog id="addPrefixModal" modal>
			<paper-toolbar>
				<h2>[[i18n.addPrefix]]</h2>
			</paper-toolbar>
			<div>
				<paper-input
					id="addPrefix"
					type="number"
					name="Prefix"
					allowed-pattern="[0-9]"
					label="[[i18n.prefix]]">
				</paper-input>
			</div>
			<div>
				<paper-input
					id="addDesc"
					name="Description"
					label="[[i18n.des]]">
				</paper-input>
			</div>
			<div>
				<paper-input
					id="addRateRow"
					type="number"
					name="PriceName"
					label="[[i18n.rate]]">
				</paper-input>
			</div>
			<div class="buttons">
				<paper-button dialog-confirm
						raised
						on-tap="_tableRow"
						class="add-button">
					<i18n-msg msgid="submit">
							Submit
					</i18n-msg>
				</paper-button>
				<paper-button
						class="cancel-button"
						on-tap="_cancel"
						dialog-dismiss>
					<i18n-msg msgid="cancel">
							Cancel
					</i18n-msg>
				</paper-button>
			</div>
		</paper-dialog>
		<paper-toast
			id="addTableRowToastError">
		</paper-toast>
		<iron-ajax
			id="addTableRowAjax"
			url="/catalogManagement/v2/pla"
			method = "POST"
			content-type="application/json"
			on-loading-changed="_onLoadingChanged"
			on-response="_addTableResponse"
			on-error="_addTableError">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-prefix-add',
			behaviors: [i18nMsgBehavior],
			_tableRow: function(event) {
				var table = document.getElementById('prefixList').table;
				var rowName = new Object();
				rowName.id = this.$.addPrefix.value;
				rowName.href = "/resourceInventoryManagement/v1/logicalResource/" + table + "/" + rowName.id;
				var resource = new Array();
				var resPre = new Object();
				resPre.name = "prefix";
				seqNum = 1;
				value = this.$.addPrefix.value;
				resPre.value = {seqNum, value};
				resource.push(resPre);
				var resDes = new Object();
				resDes.name = "description";
				seqNum = 2;
				value = this.$.addDesc.value;
				resDes.value = {seqNum, value};
				resource.push(resDes);
				var resRate = new Object();
				resRate.name = "rate";
				seqNum = 3;
				value = this.$.addRateRow.value;
				resRate.value = {seqNum, value};
				resource.push(resRate);
				rowName.resourceCharacteristic = resource;
				var ajax = this.$.addTableRowAjax;
				ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + table
				ajax.body = rowName;
				ajax.generateRequest();
				this.$.addPrefix.value = null;
				this.$.addDesc.value = null;
				this.$.addRateRow.value = null;
			},
			_addTableResponse: function(event) {
				this.$.addPrefixModal.close();
				document.getElementById("prefixGrid").clearCache();
			},
			_addTableError: function(event) {
				this.$.addTableRowToastError.text = event.detail.request.xhr.statusText;
				this.$.addTableRowToastError.open();
			},
			_cancel: function() {
				this.$.addPrefix.value = null;
				this.$.addDesc.value = null;
				this.$.addRateRow.value = null;
			},
			_onLoadingChanged: function(event) {
				if (this.$.addTableRowAjax.loading) {
					document.getElementById("progress").disabled = false;
				} else {
					document.getElementById("progress").disabled = true;
				}
			}
		});
	</script>
</dom-module>
