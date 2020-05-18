<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-dialog-scrollable/paper-dialog-scrollable.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="iron-list/iron-list.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="iron-icon/iron-icon.html">

<dom-module id="sig-ipdr-log-files-wlan">
	<template>
		<style is="custom-style">
			paper-dialog {
				overflow: auto;
			}
			paper-dialog > *:first-child {
				margin-top: 0px;
			}
			paper-toolbar {
				margin-top: 0px;
				background-color: #bc5100;
			}
			.cancel-button {
				color: black;
			}
			iron-list {
				height: 40vh;
			}
			.item {
				cursor: pointer;
				padding: 5px;
			}
		</style>
		<paper-dialog id="selectLogFileModalWlan" modal>
			<paper-toolbar>
				<h2>Select Log File</h2>
			</paper-toolbar>
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
	</template>
	<script>
		Polymer ({
			is: 'sig-ipdr-log-files-wlan',
			getLogContentWlan: function(event) {
				document.getElementById("ipdrLogListWlan").intializeGrid(event);
			},
			getLogsResponseWlan: function(event){
				this.$.logFilesWlan.items = event.detail.response;
			},
			getLogsErrorWlan: function(event) {
				this.$.addLogToastError.text = event.detail.request.xhr.statusText;
				this.$.addLogToastError.open();
			}
		});
	</script>
</dom-module>
