/**
 * Copyright 2016 - 2025 SigScale Global Inc.
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
import '@polymer/paper-dialog/paper-dialog.js';
import '@polymer/app-layout/app-toolbar/app-toolbar.js';
import '@polymer/paper-progress/paper-progress.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/paper-toggle-button/paper-toggle-button.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class subAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addServiceModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
						<paper-tab id="auth-proadd">
							<h2>Product</h2>
						</paper-tab>
						<paper-tab id="auth-add">
							<h2>Authentication</h2>
						</paper-tab>
						<paper-tab id="auth-attr">
							<h2>Authorization</h2>
						</paper-tab>
						<paper-tab id="credit-attr">
							<h2>Credit</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div id="add-divpro">
						<paper-input
								type="datetime-local"
								id="startDate"
								name="startDateTime"
								value="{{startDate}}"
								label="StartDate">
						</paper-input>
						<paper-input
								type="datetime-local"
								id="endDate"
								name="endDateTime"
								value="{{endDate}}"
								label="EndDate">
						</paper-input>
						<paper-dropdown-menu
								id="addStatus"
								value="{{lifeCycleStatus}}"
								no-animations="true"
								label="Status">
							<paper-listbox
									id="addStatusList"
									slot="dropdown-content">
								<paper-item>
									Feasibility Checked
								</paper-item>
								<paper-item>
									Designed
								</paper-item>
								<paper-item>
									Reserved
								</paper-item>
								<paper-item>
									Active
								</paper-item>
								<paper-item>
									Inactive
								</paper-item>
								<paper-item>
									Terminated
								</paper-item>
							</paper-listbox>
						</paper-dropdown-menu>
						<div>
							<paper-dropdown-menu
									id="proAddOff"
									value="{{offAddPro}}"
									label="Product Offering"
									no-animations="true"
									on-selected-item-changed="_productSelected">
								<paper-listbox
										id="addproduct10"
										slot="dropdown-content">
									<template is="dom-repeat" items="[[offers]]">
										<paper-item>
												{{item}}
										</paper-item>
									</template>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
									for="proAddOff"
									offset="0">
								Select a product from the list
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
									value="{{proAddPro}}"
									label="Product ID"
									on-focused-changed="proSelection">
							</paper-input>
							<paper-tooltip
									for="addProductId1"
									offset="0">
								Enter product ID
							</paper-tooltip>
						</div>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_serviceAddSubmit">
								Submit
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									autofocus
									on-tap="_serviceAddcancel">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="add-div">
						<div>
							<paper-input
								id="addSubscriberId"
								name="subscriber"
								value="{{idAddAuthe}}"
								label="Identity"
								required>
							</paper-input>
							<paper-tooltip
									for="addSubscriberId"
									offset="0">
								Add a service authentication ID
							</paper-tooltip>
							<div>
								<paper-checkbox
										id="idCheck"
										value="{{idCheckAdd}}"
										on-change="subcheckchanged">
									Generate
								</paper-checkbox>
							</div>
						</div>
						<div>
							<paper-input
									id="addSubscriberPassword"
									name="password"
									value="{{PassAddAuthe}}"
									label="Password">
							</paper-input>
							<paper-tooltip
									for="addSubscriberPassword"
									offset="0">
								Add a service authentication Password
							</paper-tooltip>
							<div>
								<paper-checkbox
										id="idCheckPass"
										value="{{passCheckAdd}}"
										on-change="subcheckboxchanged">
									Generate
								</paper-checkbox>
							</div>
						</div>
						<paper-input
								id="addSubscriberAkak"
								name="akak"
								maxlength="32"
								allowed-pattern="[0-9a-fA-F]"
								pattern="^[0-9a-fA-F]{32}$"
								auto-validate
								maxlength="32"
								value="{{akaAddAuthe}}"
								label="AKA K">
						</paper-input>
						<paper-tooltip
								for="addSubscriberAkak"
								offset="0">
							Add an AKA K value (32 hex chars)
						</paper-tooltip>
						<paper-input
								id="addSubscriberAkaOpc"
								name="akaOpc"
								maxlength="32"
								allowed-pattern="[0-9a-fA-F]"
								pattern="^[0-9a-fA-F]{32}$"
								auto-validate
								value="{{akaopcAddAuthe}}"
								label="AKA OPc">
						</paper-input>
						<paper-tooltip
								for="addSubscriberAkaOpc"
								offset="0">
							Add an AKA OPc value (32 hex chars)
						</paper-tooltip>
						<div class="buttons">
							<paper-button
									raised
									class="submit-button"
									on-tap="_serviceAddSubmit">
								Submit
							</paper-button>
							<paper-button
									class="cancel-button"
									dialog-dismiss
									autofocus
									on-tap="_serviceAddcancel"
									onclick="addServiceModal.close()">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="add-divone">
						<paper-input
								id="addAcctSessionInterval"
								name="acctSessionInterval"
								value="{{accSessAddAutho}}"
								label="Session Interval">
						</paper-input>
						<paper-tooltip
								for="addAcctSessionInterval"
								offset="0">
							Add a service authorization session interval
						</paper-tooltip>
						<paper-input
								id="addSessionTimeout"
								name="addSessionTimeout"
								value="{{sessTimAddAutho}}"
								label="Session Timeout">
						</paper-input>
						<paper-tooltip
								for="addSessionTimeout"
								offset="0">
							Add a service authorization session timeout
						</paper-tooltip>
						<paper-input
								id="addSubscriberClass"
								name="class"
								value="{{classAddAutho}}"
								label="Class">
						</paper-input>
						<paper-tooltip
								for="addSubscriberClass"
								offset="0">
							Add a service authorization class
						</paper-tooltip>
						<div>
							Enable
							<div>
								<paper-toggle-button
										id="addSubscriberEnabled"
										value="{{enableAddAutho}}"
										name="isServiceEnabled" checked>
								</paper-toggle-button>
							</div>
							Multisession
							<div>
								<paper-toggle-button
										id="addSubscriberMulti"
										value="{{multiAddAutho}}"
										name="multi-session">
								</paper-toggle-button>
							</div>
						</div>
						<div class="buttons">
							<paper-button dialog-confirm
									raised
									class="submit-button"
									on-tap="_serviceAddSubmit">
								Submit
							</paper-button>
							<paper-button
									dialog-dismiss
									class="cancel-button"
									on-tap="_serviceAddcancel"
									autofocus
									onclick="addServiceModal.close()">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="add-divtwo">
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
						<paper-input
								id="add8"
								name="amount"
								allowed-pattern="[0-9kmg]"
								pattern="^[0-9]+[kmg]?$"
								value="{{amoAddCre}}"
								label="Amount"
								auto-validate>
						</paper-input>
						<paper-tooltip
								for="add8"
								offset="0">
							Add balance amount
						</paper-tooltip>
						<div>
							<paper-dropdown-menu
									id="creUnits"
									value="{{uniAddCre}}"
									no-animations="true"
									label="Units">
								<paper-listbox
										id="adduni9"
										slot="dropdown-content"
										selected="1">
									<paper-item>
											Bytes
									</paper-item>
									<paper-item>
											Cents
									</paper-item>
									<paper-item>
											Seconds
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
									for="creUnits"
									offset="0">
								Select balance units (Bytes | Cents | Seconds)
							</paper-tooltip>
						</div>
						<div class="buttons">
							<paper-button dialog-confirm
									raised
									class="submit-button"
									on-tap="_serviceAddSubmit">
								Submit
							</paper-button>
							<paper-button dialog-dismiss
									class="cancel-button"
									on-tap="_serviceAddcancel"
									onclick="addServiceModal.close()">
								Cancel
							</paper-button>
						</div>
					</div>
				</iron-pages>
			</paper-dialog>
			<paper-dialog
				id="addSubscriberSecretModal"
				class="generated" modal>
				<app-toolbar>
					<h2>Server Generated</h2>
				</app-toolbar>
				<paper-item>
					<paper-item-body two-line>
						<div>
							<iron-icon icon="icons:perm-identity" item-icon></iron-icon>
								Identity:
						</div>
						<div secondary>
								[[identity]]
						</div>
					</paper-item-body>
				</paper-item>
				<paper-item>
					<paper-item-body two-line>
						<div>
							<iron-icon icon="communication:vpn-key" item-icon></iron-icon>
								Password:
						</div>
						<div secondary>
								[[password]]
						</div>
					</paper-item-body>
				</paper-item>
				<div class="close">
					<paper-button dialog-confirm autofocus>
							Close
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="addServiceAjax"
					url="/serviceInventoryManagement/v2/service"
					method = "post"
					content-type="application/json"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-loading-changed="_onLoadingChanged"
					on-response="_addServiceResponse"
					on-error="_addServiceError">
			</iron-ajax>
			<iron-ajax
					id="addProductAjax"
					url="/productInventoryManagement/v2/product"
					method = "post"
					content-type="application/json"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-loading-changed="_onLoadingChanged1"
					on-response="_addProductResponse"
					on-error="_addProductError">
			</iron-ajax>
			<iron-ajax
					id="addBucketAjax"
					method = "post"
					content-type="application/json"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-loading-changed="_onLoadingChanged2"
					on-response="_addBucketResponse"
					on-error="_addBucketError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberProductsAjax1"
					url="/catalogManagement/v2/productOffering"
					method = "GET"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updateSubscriberProductsResponse1"
					on-error="_updateSubscriberProductsError1">
			</iron-ajax>
		`;
	}
	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			selected: {
				type: Number,
				value: 0
			},
			product: {
				type: String,
			},
			proAddPro: {
				type: String
			},
			idAddAuthe: {
				type: String
			},
			PassAddAuthe: {
				type: String
			},
			akaAddAuthe: {
				type: String
			},
			akaopcAddAuthe: {
				type: String
			},
			accSessAddAutho: {
				type: String
			},
			sessTimAddAutho: {
				type: String
			},
			classAddAutho: {
				type: String
			},
			amoAddCre: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_productSelected(){
     if(this.offers[this.$.addproduct10.selected]) {
			var product = this.offers[this.$.addproduct10.selected];
			this.$.addProductId1.disabled = true
      }
	}

	proSelection() {
		if(this.proAddPro) {
			this.$.proAddOff.disabled = true
			var productId = this.proAddPro;
		}
	}

	_serviceAddSubmit(event) {
		var subscriber = new Object();
		var serviceChar = new Array();
		if(this.idAddAuthe) {
			var serviceIdentity = new Object();
			serviceIdentity.name = "serviceIdentity";
			serviceIdentity.value = this.idAddAuthe;
			serviceChar.push(serviceIdentity);
		}
		if(this.PassAddAuthe) {
			var servicePass = new Object();
			servicePass.name = "servicePassword";
			servicePass.value = this.PassAddAuthe;
			serviceChar.push(servicePass);
		} 
		if(this.akaAddAuthe) {
			var serviceAkak = new Object();
			serviceAkak.name = "serviceAkaK";
			serviceAkak.value = this.akaAddAuthe;
			serviceChar.push(serviceAkak);
		}
		if(this.akaopcAddAuthe) {
			var serviceAkaOpc = new Object();
			serviceAkaOpc.name = "serviceAkaOPc";
			serviceAkaOpc.value = this.akaopcAddAuthe;
			serviceChar.push(serviceAkaOpc);
		}
		if(this.accSessAddAutho) {
			var sessionInterval = new Object();
			sessionInterval.name = "acctSessionInterval";
			var sessionInt;
			var size = this.accSessAddAutho;
			var len = size.length;
			var m = size.charAt(len - 1);
			if(isNaN(parseInt(m))) {
				var s = size.slice(0, (len - 1));
			} else {
				var s = size;
			}
			var n = Number(s);
			if(m == "m") {
				n = n * 60;
				sessionInt = n.toString();
			} else if(m == "h") {
				n = n * 3600;
				sessionInt = n.toString();
			} else if(m == "d") {
				n = n * 86400;
				sessionInt = n.toString();
			} else {
				sessionInt = n.toString();
			}
			this.accSessAddAutho = sessionInt;
			sessionInterval.value = parseInt(this.accSessAddAutho);
			serviceChar.push(sessionInterval);
		}
		if(this.sessTimAddAutho) {
			var sessionTime = new Object();
			sessionTime.name = "sessionTimeout";
			var sessionTimeVal;
			var size = this.sessTimAddAutho;
			var len = size.length;
			var m = size.charAt(len - 1);
			if(isNaN(parseInt(m))) {
				var s = size.slice(0, (len - 1));
			} else {
				var s = size;
			}
			var n = Number(s);
			if(m == "m") {
				n = n * 60;
				sessionTimeVal = n.toString();
			} else if(m == "h") {
				n = n * 3600;
				sessionTimeVal = n.toString();
			} else if(m == "d") {
				n = n * 86400;
				sessionTimeVal = n.toString();
			} else {
				sessionTimeVal = n.toString();
			}
			this.sessTimAddAutho = sessionTimeVal;
			sessionTime.value = parseInt(this.sessTimAddAutho);
			serviceChar.push(sessionTime);
		}
		if(this.multiAddAutho) {
			var multiSess = new Object();
			multiSess.name = "multisession";
			multiSess.valueType = "boolean";
			multiSess.value = this.multiAddAutho;
			serviceChar.push(multiSess);
		}
		subscriber.serviceCharacteristic = serviceChar;
		if(this.proAddPro) {
			subscriber.product = this.proAddPro;
		}
		if(this.startDate) {
			subscriber.startDate = this.startDate;
		}
		if(this.endDate) {
			subscriber.endDate = this.endDate;
		}
		subscriber.isServiceEnabled = this.enableAddAutho.checked;
		if(this.lifeCycleStatus == "Feasibility Checked") {
			this.lifeCycleStatus = "feasibilityChecked";
		}
		if(this.lifeCycleStatus == "Designed") {
			this.lifeCycleStatus = "designed";
		}
		if(this.lifeCycleStatus == "Reserved") {
			this.lifeCycleStatus = "reserved";
		}
		if(this.lifeCycleStatus == "Active") {
			this.lifeCycleStatus = "active";
		}
		if(this.lifeCycleStatus == "Inactive") {
			this.lifeCycleStatus = "inactive"
		}
		if(this.lifeCycleStatus == "Terminated") {
			this.lifeCycleStatus = "terminated";
		}
		if(this.lifeCycleStatus) {
			subscriber.state = this.lifeCycleStatus;
		}
		var specService = new Object();
		specService.id = "1";
		specService.href = "/catalogManagement/v2/serviceSpecification/1";
		subscriber.serviceSpecification = specService;
		this.$.addServiceModal.close();
		if(subscriber.id != "") {
			var ajax = this.$.addServiceAjax;
			ajax.headers['Content-type'] = "application/json";
			ajax.body = subscriber;
			ajax.generateRequest();
			this.idAddAuthe = null;
			this.$.addSubscriberId.disabled = false;
			this.$.addSubscriberPassword.disabled = false;
			this.$.addSubscriberAkak.disabled = false;
			this.$.addSubscriberAkaOpc.disabled = false;
			this.akaAddAuthe = null;
			this.akaopcAddAuthe = null;
			this.proAddPro = null;
			this.lifeCycleStatus = null;
			this.startDate = null;
			this.endDate = null;
			this.selected = 0;
		}
	}

	_serviceAddcancel(event) {
		this.idAddAuthe = null;
		this.$.idCheck.checked = false;
		this.$.idCheckPass.checked = false;
		this.PassAddAuthe = null;
		this.$.addSubscriberAkak.disabled = null;
		this.$.addSubscriberAkaOpc.disabled = null;
		this.amoAddCre = null;
		this.$.adduni9.selected = null;
		this.proAddPro = null;
		this.lifeCycleStatus = null;
		this.startDate = null;
		this.endDate = null;
		this.$.addproduct10.selected = null
		this.$.proAddOff.disabled = false;
		this.$.addProductId1.disabled = false;
		this.selected = 0;
	}

	_addServiceResponse(event) {
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

		if(!event.detail.xhr.response.product) {
			var ajaxProduct = this.$.addProductAjax;
			var productSer = new Object();
			var productRef = new Object();
			productRef.id = this.offAddPro;
			productRef.name = this.offAddPro;
			productRef.href = "/catalogManagement/v2/productOffering/" + this.product;
			productSer.productOffering = productRef;
			var productRealizingService = new Object();
			productRealizingService.id = this.identity;
			productRealizingService.href = "/serviceInventoryManagement/v2/service/" + this.identity;
			productSer.realizingService = [productRealizingService];
			ajaxProduct.headers['Content-type'] = "application/json";
			ajaxProduct.body = productSer;
			ajaxProduct.generateRequest();
			this.$.addproduct10.selected = null
			this.$.addProductId1.disabled = false
		} else {
			var ajaxServices1 = this.$.addServiceAjax;
			var ajaxProduct1 = this.$.addProductAjax;
			var result = event.detail.xhr.response;
			var serviceId = ajaxServices1.lastResponse.id;
			var ajaxBucket = this.$.addBucketAjax;
			var bucketTop = {type: "buckettype"};
			var bunits;
			var bamount;
			if(this.amoAddCre) {
				if(this.$.adduni9.selected == 0) {
					bunits = "octets";
				} else if (this.$.adduni9.selected == 1) {
					bunits = "cents";
				} else if(this.$.adduni9.selected == 2) {
					bunits = "seconds";
				} else {
					bunits = "cents";
				}
				if(bunits && this.amoAddCre) {
					var size = this.amoAddCre;
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
						bamount = this.amoAddCre;
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
				bucketTop.product = {id: result.product,
						href: "/productInventoryManagement/v2/product/" + result.product};
				ajaxBucket.headers['Content-type'] = "application/json";
				ajaxBucket.body = bucketTop;
				ajaxBucket.url="/balanceManagement/v1/balanceAdjustment";
				ajaxBucket.generateRequest();
			}
			this.$.addServiceModal.close();
			this.amoAddCre = null;
			document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
			this.$.addproduct10.selected = null;
			this.$.proAddOff.disabled = false;
			this.$.addProductId1.disabled = false;
		}
	}

	_addProductResponse(event) {
		var ajaxServices1 = this.$.addServiceAjax;
		var ajaxProduct1 = this.$.addProductAjax;
		var result = event.detail.xhr.response;
		var serviceId = ajaxServices1.lastResponse.id;
		var ajaxBucket = this.$.addBucketAjax;
		var bucketTop = {type: "buckettype"};
		var bunits;
		var bamount;
		if(this.amoAddCre) {
			if(this.$.adduni9.selected == 0) {
				bunits = "octets";
			} else if (this.$.adduni9.selected == 1) {
				bunits = "cents";
			} else if(this.$.adduni9.selected == 2) {
				bunits = "seconds";
			} else {
				bunits = "cents";
			}
			if(bunits && this.amoAddCre) {
				var size = this.amoAddCre;
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
					bamount = this.amoAddCre;
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
				href: "/productInventoryManagement/v2/product/" + result.id};
			ajaxBucket.headers['Content-type'] = "application/json";
			ajaxBucket.body = bucketTop;
			ajaxBucket.url="/balanceManagement/v1/balanceAdjustment";
			ajaxBucket.generateRequest();
		}
		this.$.addServiceModal.close();
		this.amoAddCre = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_addServiceError(event) {
		this.$.idCheck.checked = false;
		this.$.idCheckPass.checked = false;
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
		this.$.addproduct10.selected = null
		this.$.proAddOff.disabled = false;
		this.$.addProductId1.disabled = false
	}

	_addProductError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
		var ajaxBucketRes = this.$.addBucketAjax;
		this.$.addproduct10.selected = null
		this.$.proAddOff.disabled = false;
		this.$.addProductId1.disabled = false
	}

	_addBucketResponse(event) {
		this.$.addServiceModal.close();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	subcheckboxchanged(event) {
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
	}

	subcheckchanged(event) {
		if (event.target.checked) {
			this.$.addSubscriberId.disabled = true;
		} else {
			this.$.addSubscriberId.disabled = false;
		}
	}

	_onLoadingChanged(event) {
		if (this.$.addServiceAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
	_onLoadingChanged1(event) {
		if (this.$.addProductAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
	_onLoadingChanged2(event) {
		if (this.$.addBucketAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-sub-add', subAdd);
