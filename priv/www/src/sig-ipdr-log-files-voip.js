<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-dialog-scrollable/paper-dialog-scrollable.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="iron-list/iron-list.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="iron-icon/iron-icon.html">

<dom-module id="sig-ipdr-log-files-voip">
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
		<paper-dialog id="selectLogFileModalVoip" modal>
			<paper-toolbar>
				<h2>Select Log File</h2>
			</paper-toolbar>
			<paper-dialog-scrollable>
				<iron-list id="logFilesVoip"
						as="item">
					<template>
						<div class="item" on-tap="getLogContentVoip">
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
		<iron-ajax id="getLogsAjaxVoip"
				method = "GET"
				headers='{"Accept": "application/json"}'
				on-response="getLogsResponseVoip"
				on-error="getLogsErrorVoip">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-ipdr-log-files-voip',
			getLogContentVoip: function(event) {
				document.getElementById("ipdrLogListVoip").intializeGrid(event);
			},
			getLogsResponseVoip: function(event){
				this.$.logFilesVoip.items = event.detail.response;
			},
			getLogsErrorVoip: function(event) {
				this.$.addLogToastError.text = event.detail.request.xhr.statusText;
				this.$.addLogToastError.open();
			}
		});
	</script>
</dom-module>
