<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="vaadin-grid/vaadin-grid.html">
<link rel="import" href="vaadin-grid/vaadin-grid-filter.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="paper-fab/paper-fab.html" >
<link rel="import" href="paper-toast/paper-toast.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="sig-user-add.html">
<link rel="import" href="sig-user-update.html">

<dom-module id="sig-user-list">
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
				height: 100vh;
				font-size: inherit;
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
		<vaadin-grid
				id="userGrid"
				active-item="{{activeItem}}">
			<vaadin-grid-column width="15ex" flex-grow="5">
				<template class="header">
					<vaadin-grid-filter id="filterId" aria-label="[[i18n.userName]]" path="id" value="{{_filterId}}">
						<input placeholder="[[i18n.userName]]" value="{{_filterId::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.id]]</template>
			</vaadin-grid-column>
			<vaadin-grid-column width="15ex" flex-grow="5">
				<template class="header">
					<vaadin-grid-filter id="filterLanguage" aria-label="[[i18n.language]]" path="language" value="{{_filterLanguage}}">
						<input placeholder="[[i18n.language]]" value="{{_filterLanguage::input}}" focus-target>
					</vaadin-grid-filter>
				</template>
				<template>[[item.language]]</template>
			</vaadin-grid-column>
		</vaadin-grid>
		<div class="add-button">
		<paper-fab
				icon="add"
				on-tap="showAddUserModal">
		</paper-fab>
		</div>
		<paper-toast
            id="userToastError">
      </paper-toast>
		<paper-toast
				id="addUserToastErrorForm"
				text="[[i18n.validateToastError]]">
		</paper-toast>
		<paper-toast
				id="addUserToastSuccess"
				text="[[i18n.userAdded]]">
		</paper-toast>
		<paper-toast
				id="updateUserToastSuccess"
				text="[[i18n.userUpdated]]">
		</paper-toast>
		<paper-toast
				id="deleteUserToastSuccess"
				text="[[i18n.userDeleted]]">
		</paper-toast>
		<paper-toast id="userErrorToast" duration="0">
			<paper-button
					class="yellow-button"
					onclick="userErrorToast.toggle()">
				Close
			</paper-button>
		</paper-toast>
		<iron-ajax
				id="getUserAjax"
				url="/partyManagement/v1/individual"
				rejectWithRequest>
		</iron-ajax>
	</template>
	<script>
		var cbUsers;
		var etag;
		var lastItem;
		Polymer ({
			is: 'sig-user-list',
			behaviors: [i18nMsgBehavior],
			properties: {
				activePage: {
					type: Boolean,
					value: false,
					observer: '_activePageChanged'
				},
				activeItem: {
					observer:'_activeItemChanged'
				},
				_filterId: {
					observer: '_filterChangedId'
				}
			},
			_activePageChanged: function(active) {
				if (active) {
					var grid = this.$.userGrid;
					grid.columns = [
						{
							name: "id"
						},
						{
							name: "language"
						}
					];
					grid.dataProvider = this._getUsers;
				}
			},
			_filterChangedId: function(filter) {
				this.etag = null;
				delete this.$.getUserAjax.headers['If-Range'];
				this.$.userGrid.size = 0;
			},
			_activeItemChanged: function(item) {
				if (item != null){
					this.$.userGrid.selectedItems = item ? [item] : [];
					var modal = document.getElementById("updateUserModal");
					modal.open();
					document.getElementById("updateUserName").value = item.id;
					document.getElementById("updateUserPassword").value = item.password;
				}
			},
			_getUsers: function(params, callback) {
				var grid = document.getElementById('userGrid');
				var ajax = document.getElementById("getUserAjax");
				delete ajax.params['filter'];
				function checkHead(param) {
					return param.path == "id";
				}
				params.filters.filter(checkHead).forEach(function(filter) {
					if (filter.value) {
						ajax.params['filter'] = "\"[{id.like=[" + filter.value + "%";
					}
				});
				function checkChar(param) {
					return param.path != "id";
				}
				params.filters.filter(checkChar).forEach(function(filter) {
					if (filter.value) {
							if (!ajax.params['filter']) {
								ajax.params['filter'] = "\"[{";
							} else {
								ajax.params['filter'] += "],";
							}
							ajax.params['filter'] += "characteristic.contains=[{name=language" + ",value.like=[" + filter.value + "%]}";
					}
				});
				if (ajax.params['filter']) {
					ajax.params['filter'] += "]}]\"";
				}
				var userList = document.getElementById('userList');
				var handleAjaxResponse = function(request) {
					if (request) {
						userList.etag = request.xhr.getResponseHeader('ETag');
						var range = request.xhr.getResponseHeader('Content-Range');
						var range1 = range.split("/");
						var range2 = range1[0].split("-");
						if (range1[1] != "*") {
							grid.size = Number(range1[1]);
						} else {
							grid.size = Number(range2[1]) + grid.pageSize * 2;
						}
						var vaadinItems = new Array();
						function checkChar(characteristic){
							return characteristic.name == "locale";
						}
						for (var index in request.response) {
							var newRecord = new Object();
							newRecord.id = request.response[index].id;
							var langChar = request.response[index].characteristic.find(checkChar);
							if (langChar != undefined) {
								newRecord.language = langChar.value;
							}
							vaadinItems[index] = newRecord;
						}
						callback(vaadinItems);
					} else {
						grid.size = 0;
						callback([]);
					}
				};
				var handleAjaxError = function(error) {
					userList.etag = null;
					var toast = document.getElementById('userToastError');
					toast.text = error;
					toast.open();
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
						if (userList.etag && params.page > 0) {
							ajax.headers['If-Range'] = userList.etag;
						} else {
							delete ajax.headers['If-Range'];
						}
						return ajax.generateRequest().completes;
					}, handleAjaxError).then(handleAjaxResponse, handleAjaxError);
				} else {
					var startRange = params.page * params.pageSize + 1;
					var endRange = startRange + params.pageSize - 1;
					ajax.headers['Range'] = "items=" + startRange + "-" + endRange;
					if (userList.etag && params.page > 0) {
						ajax.headers['If-Range'] = userList.etag;
					} else {
						delete ajax.headers['If-Range'];
					}
					ajax.generateRequest().completes.then(handleAjaxResponse, handleAjaxError);
				}
			},
			showAddUserModal: function(event) {
				document.getElementById("addUserModal").open();
			},
		});
	</script>
</dom-module>
