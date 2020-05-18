<!--  vim: set ts=3:  -->
<link rel="import" href="polymer/polymer.html">
<link rel="import" href="i18n-msg/i18n-msg.html">
<link rel="import" href="i18n-msg/i18n-msg-behavior.html">
<link rel="import" href="paper-dialog/paper-dialog.html">
<link rel="import" href="paper-toolbar/paper-toolbar.html">
<link rel="import" href="paper-tabs/paper-tabs.html">
<link rel="import" href="paper-input/paper-input.html">
<link rel="import" href="paper-item/paper-icon-item.html">
<link rel="import" href="paper-item/paper-item-body.html">
<link rel="import" href="paper-tooltip/paper-tooltip.html">
<link rel="import" href="paper-button/paper-button.html">
<link rel="import" href="paper-toggle-button/paper-toggle-button.html" >
<link rel="import" href="paper-toast/paper-toast.html">
<link rel="import" href="paper-styles/color.html">
<link rel="import" href="iron-ajax/iron-ajax.html">
<link rel="import" href="iron-pages/iron-pages.html">
<link rel="import" href="paper-checkbox/paper-checkbox.html">
<link rel="import" href="iron-icons/iron-icons.html">
<link rel="import" href="iron-icons/communication-icons.html">

<dom-module id="sig-sub-add">
	<template>
		<style is="custom-style">
			paper-dialog {
				overflow: auto;
			}
			paper-input {
				--paper-input-container-focus-color: var(--paper-yellow-900);
			}
			paper-toolbar {
				margin-top: 0px;
				color: white;
				background-color: #bc5100;
			}
			paper-item-body {
				--paper-item-body-secondary: {
					font-weight: bold;
					font-size: larger;
				}
			}
			paper-toast.error {
				background-color: var(--paper-red-a400);
			}
			paper-toggle-button {
				--paper-toggle-button-checked-bar-color: #ffb04c;
				--paper-toggle-button-checked-button-color: var(--paper-yellow-900);
			}
			paper-checkbox {
				--paper-checkbox-checked-color: #ffb04c;
				--paper-checkbox-checkmark-color: var(--paper-yellow-900);
			}
			.toggle {
				display:inline-block;
			}
			.generate {
				display: inline-block;
				width: 8em;
			}
			.identity {
				display: inline-block;
			}
			.password {
				display: inline-block;
			}
			.add-button {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 8em;
			}
			.cancel-button {
				color: black;
			}
			.generated {
				padding: 10px;
				overflow: auto;
			}
			.close {
				background-color: var(--paper-lime-a700);
				color: black;
				float: right;
				width: 5em;
			}
		</style>
		<paper-dialog id="addServiceModal" modal>
			<paper-toolbar>
				<paper-tabs selected="{{selected}}">
					<paper-tab id="auth-proadd">
						<h2>[[i18n.subProduct]]</h2>
					</paper-tab>
					<paper-tab id="auth-add">
						<h2>[[i18n.AuthTitle]]</h2>
					</paper-tab>
					<paper-tab id="auth-attr">
						<h2>[[i18n.AuthorTitle]]</h2>
					</paper-tab>
					<paper-tab id="credit-attr">
						<h2>[[i18n.credit]]</h2>
					</paper-tab>
				</paper-tabs>
			</paper-toolbar>
			<iron-pages selected="{{selected}}">
			<div id="add-divpro">
				<div>
					<paper-dropdown-menu
						id="addproductDrop10"
						on-selected-item-changed="_productSelected"
						label="[[i18n.offers]]">
						<paper-listbox
							id="addproduct10"
							slot="dropdown-content"
							class="dropdown-content">
							<template is="dom-repeat" items="[[offers]]">
								<paper-item>
									{{item}}
								</paper-item>
							</template>
						</paper-listbox>
					</paper-dropdown-menu>
					<paper-tooltip>
						Select the offer.
					</paper-tooltip>
				</div>
				<div>
					<iron-a11y-keys id="a11y"
						target="[[target]]"
						keys="enter"
						on-keys-pressed="onEnter">
					</iron-a11y-keys>
					<paper-input
						id="addProductId1"
						name="product"
						label="[[i18n.prodId]]">
					</paper-input>
					<paper-tooltip>
						Product Id.
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button
							raised
							class="add-button"
							on-tap="_serviceAddSubmit">
						<i18n-msg msgid="submit">
								Submit
						</i18n-msg>
					</paper-button>
					<paper-button
							dialog-dismiss
							class="cancel-button"
							autofocus
							on-tap="_serviceAddcancel"
							onclick="addServiceModal.close()">
					<i18n-msg msgid="cancel">
						Cancel
					</i18n-msg>
					</paper-button>
				</div>
			</div>
			<div id="add-div">
				<div>
					<paper-input
							id="addSubscriberId"
							class="identity"
							name="subscriber"
							label="[[i18n.identity]]"
							required>
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="subTool">
								Identity, or username, of the subscriber to authenticate.
						</i18n-msg>
					</paper-tooltip>
					<div class="generate">
						<paper-checkbox
							id="idCheck"
							on-change="subcheckchanged">
							<i18n-msg msgid="gen">
									Generate
							</i18n-msg>
						</paper-checkbox>
					</div>
				</div>
				<div>
					<paper-input
							id="addSubscriberPassword"
							class="password"
							name="password"
							label="[[i18n.password]]">
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="passwordTooltip">
								Secret password shared only with the subscriber
						</i18n-msg>
					</paper-tooltip>
					<div class="generate">
						<paper-checkbox
							id="idCheckPass"
							on-change="subcheckboxchanged">
							<i18n-msg msgid="gen">
									Generate
							</i18n-msg>
						</paper-checkbox>
					</div>
				</div>
				<div>
					<paper-input
							id="addSubscriberAkak"
							class="identity"
							name="akak"
							maxlength="32"
							allowed-pattern="[0-9a-fA-F]"
							pattern="^[0-9a-fA-F]{32}$"
							auto-validate
							maxlength="32"
							label="[[i18n.akak]]">
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="akakTooltip">
								Service akak
						</i18n-msg>
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="addSubscriberAkaOpc"
							class="identity"
							name="akaOpc"
							maxlength="32"
							allowed-pattern="[0-9a-fA-F]"
							pattern="^[0-9a-fA-F]{32}$"
							auto-validate
							label="[[i18n.akaOpc]]">
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="akaOpcTooltip">
								Service akaOpc
						</i18n-msg>
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button
							raised
							class="add-button"
							on-tap="_serviceAddSubmit">
						<i18n-msg msgid="submit">
								Submit
						</i18n-msg>
					</paper-button>
					<paper-button
							dialog-dismiss
							class="cancel-button"
							autofocus
							on-tap="_serviceAddcancel"
							onclick="addServiceModal.close()">
					<i18n-msg msgid="cancel">
						Cancel
					</i18n-msg>
					</paper-button>
				</div>
			</div>
			<div id="add-divone">
				<div>
					<paper-input
							id="addAcctSessionInterval"
							name="acctSessionInterval"
							label="[[i18n.sessionInterval]]">
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="sessionTimeoutTool">
								Time between authorization requests in an active session in seconds.
						</i18n-msg>
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							id="addSessionTimeout"
							name="addSessionTimeout"
							label="[[i18n.sessionTimeout]]">
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="acctInterimIntervalTool">
								Time between accounting interim updates in seconds.
						</i18n-msg>
					</paper-tooltip>
				</div>
					<paper-input
							id="addSubscriberClass"
							name="class"
							label="[[i18n.class]]">
					</paper-input>
				<div>
					<i18n-msg msgid="enable">
							Enable
					</i18n-msg>
					<div style="display:inline-block;">
						<paper-toggle-button
							id="addSubscriberEnabled"
							name="isServiceEnabled" checked>
						</paper-toggle-button>
						<paper-tooltip>
							<i18n-msg msgid="enableTool">
									Temporarily enable, or disable, authorization for service.
							</i18n-msg>
						</paper-tooltip>
					</div>
					<i18n-msg msgid="multi">
							Multisession
					</i18n-msg>
					<div style="display:inline-block;">
						<paper-toggle-button
							id="addSubscriberMulti"
							name="multi-session">
						</paper-toggle-button>
						<paper-tooltip>
							<i18n-msg msgid="multiTool">
									Allow multiple simultaneous sessions.
							</i18n-msg>
						</paper-tooltip>
					</div>
				</div><br>
					<div class="buttons">
						<paper-button dialog-confirm
								raised
								class="add-button"
								on-tap="_serviceAddSubmit">
							<i18n-msg msgid="submit">
									Submit
							</i18n-msg>
						</paper-button>
						<paper-button
								dialog-dismiss
								class="cancel-button"
								on-tap="_serviceAddcancel"
								autofocus
								onclick="addServiceModal.close()">
							<i18n-msg msgid="cancel">
									Cancel
							</i18n-msg>
						</paper-button>
					</div>
				</div>
			<div id=add-divtwo>
				<div>
					<paper-input
							id="AddUpdatePro"
							name="product"
							label="[[i18n.prod]]"
							disabled
							hidden>
					</paper-input>
				</div>
				<div>
					<paper-input
							id="AddUpdateProId"
							name="productId"
							label="[[i18n.prodId]]"
							disabled
							hidden>
					</paper-input>
				</div>
				<div>
					<paper-input
							id="add8"
							name="amount"
							allowed-pattern="[0-9kmg]"
							pattern="^[0-9]+[kmg]?$"
							label="[[i18n.amount]]"
							auto-validate>
					</paper-input>
					<paper-tooltip>
						<i18n-msg msgid="addSubAmountTooltip">
								Subscriber balance amount.
						</i18n-msg>
					</paper-tooltip>
				</div>
				<div>
					<paper-dropdown-menu
							label="[[i18n.unit]]">
							<paper-listbox
								id="adduni9"
								slot="dropdown-content"
								class="dropdown-content"
								selected="1">
								<paper-item>
									<i18n-msg msgid="bytes">
										Bytes
									</i18n-msg>
								</paper-item>
								<paper-item>
									<i18n-msg msgid="cents">
										Cents
									</i18n-msg>
								</paper-item>
								<paper-item>
									<i18n-msg msgid="seconds">
										Seconds
									</i18n-msg>
								</paper-item>
							</paper-listbox>
					</paper-dropdown-menu>
					<paper-tooltip>
						<i18n-msg msgid="addSubUnitTooltip">
								Subscriber balance units.
						</i18n-msg>
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							class="add-button"
							on-tap="_serviceAddSubmit">
						<i18n-msg msgid="submit">
								Submit
						</i18n-msg>
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							on-tap="_serviceAddcancel"
							onclick="addServiceModal.close()">
						<i18n-msg msgid="cancel">
								Cancel
						</i18n-msg>
					</paper-button>
				</div>
			</div>
			</iron-pages>
			<paper-toast
					id="addSubscriberToastError">
			</paper-toast>
		</paper-dialog>
		<paper-dialog
				id="addSubscriberSecretModal"
				class="generated" modal>
			<paper-toolbar>
				<h2>[[i18n.serverGen]]</h2>
			</paper-toolbar>
			<paper-icon-item>
				<paper-item-body two-line>
					<div>
						<iron-icon icon="icons:perm-identity" item-icon></iron-icon>
								[[i18n.identity]]
					</div>
					<div secondary>
								[[identity]]
					</div>
				</paper-item-body>
			</paper-icon-item>
			<paper-icon-item>
				<paper-item-body two-line>
					<div>
						<iron-icon icon="communication:vpn-key" item-icon></iron-icon>
								[[i18n.password]]
					</div>
					<div secondary>
								[[password]]
					</div>
				</paper-item-body>
			</paper-icon-item>
			<div class="close">
				<paper-button dialog-confirm autofocus>
					<i18n-msg msgid="close">
							Close
					</i18n-msg>
				</paper-button>
			</div>
		</paper-dialog>
		<iron-ajax
				id="addServiceAjax"
				url="/serviceInventoryManagement/v2/service"
				method = "post"
				content-type="application/json"
				on-loading-changed="_onLoadingChanged"
				on-response="_addServiceResponse"
				on-error="_addServiceError">
		</iron-ajax>
		<iron-ajax
				id="addProductAjax"
				url="/productInventoryManagement/v2/product"
				method = "post"
				content-type="application/json"
				on-loading-changed="_onLoadingChanged"
				on-response="_addProductResponse"
				on-error="_addProductError">
		</iron-ajax>
		<iron-ajax
				id="addBucketAjax"
				method = "post"
				content-type="application/json"
				on-loading-changed="_onLoadingChanged"
				on-response="_addBucketResponse"
				on-error="_addBucketError">
		</iron-ajax>
		<iron-ajax
				id="updateSubscriberProductsAjax1"
				url="/catalogManagement/v2/productOffering"
				method = "GET"
				on-response="_updateSubscriberProductsResponse1"
				on-error="_updateSubscriberProductsError1">
		</iron-ajax>
	</template>
	<script>
		Polymer ({
			is: 'sig-sub-add',
			behaviors: [i18nMsgBehavior],
			properties: {
				selected: {
					type: Number,
					value: 1
				},
				product: {
					type: String,
				},
				productId: {
					type: String,
				}
			},
			_productSelected: function() {
				var product = this.offers[this.$.addproduct10.selected];
				if(product) {
					document.getElementById("AddUpdatePro").value = product;
					document.getElementById("subscriberAdd").product = product;
				}
			},
			onEnter: function() {
				var productId = this.$.addProductId1.value;
				if(productId) {
					document.getElementById("AddUpdateProId").value = productId;
					document.getElementById("subscriberAdd").productId = productId;
					this.$.addProductId1.value = null;
				}
			},
			_serviceAddSubmit: function(event) {
				var subscriber = new Object();
				var serviceChar = new Array();
				if(this.$.addSubscriberId.value) {
					var serviceIdentity = new Object();
					serviceIdentity.name = "serviceIdentity";
					serviceIdentity.value = this.$.addSubscriberId.value;
					serviceChar.push(serviceIdentity);
				}
				if(this.$.addSubscriberPassword.value) {
					var servicePass = new Object();
					servicePass.name = "servicePassword";
					servicePass.value = this.$.addSubscriberPassword.value;
					serviceChar.push(servicePass);
				} 
				if(this.$.addSubscriberAkak.value) {
					var serviceAkak = new Object();
					serviceAkak.name = "serviceAkaK";
					serviceAkak.value = this.$.addSubscriberAkak.value;
					serviceChar.push(serviceAkak);
				}
				if(this.$.addSubscriberAkaOpc.value) {
					var serviceAkaOpc = new Object();
					serviceAkaOpc.name = "serviceAkaOPc";
					serviceAkaOpc.value = this.$.addSubscriberAkaOpc.value;
					serviceChar.push(serviceAkaOpc);
				}
				if(parseInt(this.$.addAcctSessionInterval.value)) {
					var sessionInterval = new Object();
					sessionInterval.name = "acctSessionInterval";
					sessionInterval.value = parseInt(this.$.addAcctSessionInterval.value);
					serviceChar.push(sessionInterval);
				}
				if(parseInt(this.$.addSessionTimeout.value)) {
					var sessionTime = new Object();
					sessionTime.name = "sessionTimeout";
					sessionTime.value = parseInt(this.$.addSessionTimeout.value);
					serviceChar.push(sessionTime);
				}
				if(this.$.addSubscriberMulti.checked) {
					var multiSess = new Object();
					multiSess.name = "multisession";
					multiSess.valueType = "boolean";
					multiSess.value = this.$.addSubscriberMulti.checked;
					serviceChar.push(multiSess);
				}
				subscriber.serviceCharacteristic = serviceChar;
				subscriber.isServiceEnabled = this.$.addSubscriberEnabled.checked;
				subscriber.state = "active";
				var specService = new Object();
				specService.id = "1";
				specService.href = "/catalogManagement/v2/serviceSpecification/1";
				subscriber.serviceSpecification = specService;
				if(subscriber.id != "") {
					var ajax = this.$.addServiceAjax;
					ajax.headers['Content-type'] = "application/json";
					ajax.body = subscriber;
					ajax.generateRequest();
					this.$.addSubscriberId.value = null;
					this.$.addSubscriberId.disabled = false;
					this.$.addSubscriberPassword.disabled = false;
					this.$.addSubscriberAkak.disabled = false;
					this.$.addSubscriberAkaOpc.disabled = false;
					this.$.addSubscriberAkak.value = null;
					this.$.addSubscriberAkaOpc.value = null;
					this.$.addproduct10.selected= null;
					this.$.addProductId1.value = null;
				}
			},
			_serviceAddcancel: function(event) {
				this.$.addSubscriberId.value = null;
				this.$.idCheck.checked = false;
				this.$.idCheckPass.checked = false;
				this.$.addSubscriberPassword.value = null;
				this.$.addSubscriberAkak.disabled = null;
				this.$.addSubscriberAkaOpc.disabled = null;
				this.$.add8.value = null;
				this.$.adduni9.selected = null;
				this.$.addProductId1.value = null;
			},
			_addServiceResponse: function(event) {
				this.identity = event.detail.xhr.response.id;
				function checkPasswordResp(pass) {
					return pass.name == "servicePassword";
				}
				var indexPass = event.detail.xhr.response.serviceCharacteristic.findIndex(checkPasswordResp);
				if(indexPass != 1) {
					this.password = "";
				} else {
					this.password = event.detail.xhr.response.serviceCharacteristic[indexPass].value;
				}
				if(this.$.idCheck.checked || this.$.idCheckPass.checked) {
					this.$.idCheck.checked = false;
					this.$.idCheckPass.checked = false;
					this.$.addSubscriberSecretModal.open();
				}
				if(!this.productId) {
					var ajaxProduct = this.$.addProductAjax;
					var productSer = new Object();
					var productRef = new Object();
					productRef.id = this.product;
					productRef.name = this.product;
					productRef.href = "/catalogManagement/v2/productOffering/" + this.product;
					productSer.productOffering = productRef;
					var productRealizingService = new Object();
					productRealizingService.id = this.identity;
					productRealizingService.href = "/serviceInventoryManagement/v2/service/" + this.identity;
					productSer.realizingService = [productRealizingService];
					ajaxProduct.headers['Content-type'] = "application/json";
					ajaxProduct.body = productSer;
					ajaxProduct.generateRequest();
				}
			},
			_addServiceError: function(event) {
				this.$.idCheck.checked = false;
				this.$.idCheckPass.checked = false;
				this.$.addSubscriberToastError.text = event.detail.request.xhr.statusText;
				this.$.addSubscriberToastError.open();
			},
			_addProductResponse: function(event) {
				var ajaxServices1 = this.$.addServiceAjax;
				var ajaxProduct1 = this.$.addProductAjax;
				var result = event.detail.xhr.response;
				serviceId = ajaxServices1.lastResponse.id;
				var ajaxBucket = this.$.addBucketAjax;
				var bucketTop = {name: "channel"};
				var bunits;
				var bamount;
				if(this.$.add8.value) {
					if(this.$.adduni9.selected == 0) {
						bunits = "octets";
					} else if (this.$.adduni9.selected == 1) {
						bunits = "cents";
					} else if(this.$.adduni9.selected == 2) {
						bunits = "seconds";
					} else {
						bunits = "cents";
					}
					if(bunits && this.$.add8.value) {
						var size = this.$.add8.value;
						var len = size.length;
						var m = size.charAt(len - 1);
						if(isNaN(parseInt(m))) {
							var s = size.slice(0, (len - 1));
						} else {
							var s = size;
						}
						if(bunits == "octets") {
							if(m == "m") {
								bamount = s + "000000b";
							} else if(m == "g") {
								bamount = s + "000000000b";
							} else if(m == "k") {
								bamount = s + "000b";
							} else {
								bamount = s + "b";
							}
						} else if(bunits == "cents") {
							bamount = this.$.add8.value;
						} else if(bunits == "seconds") {
							var n = Number(s);
							if(m == "m") {
								n = n * 60;
								bamount = n.toString() + "s";
							} else if(m == "h") {
								n = n * 3600;
								bamount = n.toString() + "s";
							} else {
								bamount = n.toString() + "s";
							}
						}
						bucketTop.amount = {units: bunits, amount: bamount};
					}
					bucketTop.product = {id: result.id,
							href: "/productInventoryManagement/v2/product/" + this.product};
					ajaxBucket.headers['Content-type'] = "application/json";
					ajaxBucket.body = bucketTop;
					ajaxBucket.url="/balanceManagement/v1/product/" + result.id + "/balanceTopup";
					ajaxBucket.generateRequest();
				}
				this.$.addServiceModal.close();
				this.$.add8.value = null;
			},
			_addProductError: function(event) {
				var ajaxBucketRes = this.$.addBucketAjax;
			},
			_addBucketResponse: function(event) {
				this.$.addServiceModal.close();
				document.getElementById("subscriberGrid").clearCache();
				document.getElementById("addSubscriberToastSuccess").open();
			},
			_addBucketError: function(event) {
			},
			_onLoadingChanged: function(event) {
				if (this.$.addServiceAjax.loading) {
					document.getElementById("progress").disabled = false;
				} else {
					document.getElementById("progress").disabled = true;
				}
			},
			subcheckboxchanged: function(event) {
				if (event.target.checked) {
					this.$.addSubscriberAkak.disabled = true;
					this.$.addSubscriberAkaOpc.disabled = true;
					this.$.addSubscriberPassword.disabled = true;
					this.$.addSubscriberAkak.disabled = true;
					this.$.addSubscriberAkaOpc.disabled = true;
				} else {
					this.$.addSubscriberAkak.disabled = false;
					this.$.addSubscriberAkaOpc.disabled = false;
					this.$.addSubscriberPassword.disabled = false;
					this.$.addSubscriberAkak.disabled = false;
					this.$.addSubscriberAkaOpc.disabled = false;
				}
			},
			subcheckchanged: function(event) {
				if (event.target.checked) {
					this.$.addSubscriberId.disabled = true;
				} else {
					this.$.addSubscriberId.disabled = false;
				}
			}
		});
	</script>
</dom-module>
