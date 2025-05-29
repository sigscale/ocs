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
import '@polymer/paper-input/paper-input.js';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-dropdown-menu/paper-dropdown-menu.js';
import '@polymer/paper-listbox/paper-listbox.js';
import '@polymer/paper-item/paper-item.js'
import '@polymer/paper-checkbox/paper-checkbox.js'
import '@polymer/paper-tabs/paper-tabs.js';
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-toggle-button/paper-toggle-button.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class subUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateSubscriberModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
						<paper-tab id="product-up">
							<h2>Product</h2>
						</paper-tab>
						<paper-tab id="authen">
							<h2>Authentication</h2>
						</paper-tab>
						<paper-tab id="autho">
							<h2>Authorization</h2>
						</paper-tab>
						<paper-tab id="credit-up">
							<h2>Credit</h2>
						</paper-tab>
					</paper-tabs>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<iron-pages
						selected="{{selected}}">
					<div id="edit-product">
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
						<paper-input
								id="updateProduct"
								name="product"
								value="{{updateSubProduct}}"
								label="Product">
						</paper-input>
						<paper-tooltip
								for="updateProduct"
								offset="0">
							Product Id
						</paper-tooltip>
						<div class="buttons">
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberProduct"
									class="update-button">
								Update
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="deleteSubscriber"
									class="delete-button">
								Delete
							</paper-button>
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="edit-password" >
						<paper-input
								id="updateSubscriberId"
								name="id"
								label="Identity"
								value="{{updateSubId}}"
								disabled>
						</paper-input>
						<paper-input
								id="updateSubscriberPassword"
								name="password"
								value="{{updateSubPass}}"
								label="Secret"
								disabled>
						</paper-input>
						<paper-input
								id="updateSubscriberNewPassword"
								name="newpassword"
								value="{{updateSubNPass}}"
								label="New Password"
								required
								auto-validate
								error-message="Wrong Password">
						</paper-input>
						<paper-tooltip
								for="updateSubscriberNewPassword"
								offset="0">
							Add a new value to update the service authentication password
						</paper-tooltip>
						<div class="buttons">
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberAuthentication"
									class="update-button">
								Update
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="deleteSubscriber"
									class="delete-button">
								Delete
							</paper-button>
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="edit-attributes">
						<paper-input
								id="updateSubscriberTimeout"
								name="sessionTimeout"
								value="{{updateSubSes}}"
								label="Session Timeout">
						</paper-input>
						<paper-tooltip
								for="updateSubscriberTimeout"
								offset="0">
							Add a new value to update the service authorization timeout
						</paper-tooltip>
						<paper-input
								id="updateSubscriberInterval"
								name="acctSessionInterval"
								value="{{updateSubSessInt}}"
								label="Accounting Interval">
						</paper-input>
						<paper-tooltip
								for="updateSubscriberInterval"
								offset="0">
							Add a new value to update the service authorization interval
						</paper-tooltip>
						<div>
							<paper-input
									id="updateSubscriberClass"
									name="class"
									value="{{updateSubClass}}"
									type="text"
									label="Class">
							</paper-input>
							<paper-tooltip
									for="updateSubscriberClass"
									offset="0">
								Add a new value to update the service authorization class
							</paper-tooltip>
						</div>
						<div>
								Enable
							<div>
								<paper-toggle-button
										id="updateSubscriberEnabled"
										value="{{updateSubEn}}">
								</paper-toggle-button>
							</div>
							Multisession
							<div>
								<paper-toggle-button
										id="updateSubscriberMulti"
										value="{{updateSubMul}}">
								</paper-toggle-button>
							</div>
						</div>
						<div class="buttons">
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberAuthorization"
									class="update-button">
								Update
							</paper-button>
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
						</div>
					</div>
					<div id="edit-bal">
						<paper-input
								id="updatePro"
								name="product"
								value="{{updateSubPro}}"
								label="Product"
								disabled>
						</paper-input>
						<paper-input
								id="edit-amount"
								name="amount"
								value="{{updateSubAmo}}"
								allowed-pattern="[0-9kmgdhs]"
								pattern="^[0-9]+[kmgdhs]?$"
								label="Amount">
						</paper-input>
						<paper-tooltip
								for="edit-amount"
								offset="0">
							Add a new value to update the service credit amount
						</paper-tooltip>
						<div>
							<paper-dropdown-menu
									id="updateUni"
									value="{{updateSubUni}}"
									no-animations="true"
									label="Units">
								<paper-listbox
										id="updateUni1"
										slot="dropdown-content">
									<paper-item value="octets">
											Bytes
									</paper-item>
									<paper-item value="cents">
											Cents
									</paper-item>
									<paper-item value="seconds">
											Seconds
									</paper-item>
								</paper-listbox>
							</paper-dropdown-menu>
							<paper-tooltip
									for="updateUni"
									offset="0">
								Select a new value to update the service credit units (Bytes | Cents | Seconds)
							</paper-tooltip>
						</div>
						<div class="buttons">
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberBalance"
									class="update-button">
								Update
							</paper-button>
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
						</div>
					</div>
				</iron-pages>
			</paper-dialog>
			<iron-ajax
					id="updateSubscriberAuthenticationAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updateSubscriberAuthenticationResponse"
					on-error="_updateSubscriberAuthenticationError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberAuthorizationAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updateSubscriberAuthorizationResponse"
					on-error="_updateSubscriberAuthorizationError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberProductAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updateSubscriberProductResponse"
					on-error="_updateSubscriberProductError">
			</iron-ajax>
			<iron-ajax
					id="deleteSubscriberAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_deleteSubscriberResponse"
					on-error="_deleteSubscriberError">
			</iron-ajax>
			<iron-ajax
					id="getServiceRespAjax"
					headers='{"Accept": "application/json, application/problem+json"}'
					method = "GET">
			</iron-ajax>
			<iron-ajax
					id="addBucketAjax"
					method = "post"
					headers='{"Accept": "application/json, application/problem+json"}'
					content-type="application/json">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberProductsAjax"
					url="/catalogManagement/v2/productOffering"
					method = "GET"
					headers='{"Accept": "application/json, application/problem+json"}'
					on-response="_updateSubscriberProductsResponse"
					on-error="_updateSubscriberProductsError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			selected: {
				type: Number,
				value: 0
			},
			offers: {
				type: Array,
				value: function() {
					return [];
				}
			},
			etag: {
				type: String,
				value: null
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			loading: {
				type: Boolean,
				value: false
			},
			valCha: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			valChaOne: {
				type: Array,
				readOnly: true,
				notify: true,
				value: function() {
					return []
				}
			},
			updateSubPass: {
				type: String
			},
			updateSubNPass: {
				type: String
			},
			updateSubSes: {
				type: String
			},
			updateSubSessInt: {
				type: String
			},
			updateSubClass: {
				type: String
			},
			updateSubAmo: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item, last) {
		if(item || last) {
			var current;
			if(item) {
				current = item;
			} else if(last) {
				current = last;
			}
			var ajaxCh = this.$.getServiceRespAjax;
			ajaxCh.url = "/serviceInventoryManagement/v2/service/" + current.id;
			ajaxCh.generateRequest();
			this.$.updateSubscriberProductsAjax.generateRequest();
			this.updateSubId = current.id;
			this.updateSubPass = current.password;
			this.updateSubPro = current.product;
			this.updateSubSes = current.sessionTimeout;
			this.updateSubSessInt = current.acctInterimInterval;
			this.updateSubClass =  current.class;
			this.$.updateSubscriberEnabled.checked =  current.enabled;
			this.$.updateSubscriberMulti.checked =  current.multisession;
			this.updateSubProduct = current.product;
			switch(current.state) {
				case "feasibilityChecked":
					this.lifeCycleStatus = "Feasibility Checked"; 
					break;
				case "designed":
					this.lifeCycleStatus = "Designed";
					break;
				case "reserved":
					this.lifeCycleStatus = "Reserved";
					break;
				case "active":
					this.lifeCycleStatus = "Active";
					break;
				case "inactive":
					this.lifeCycleStatus = "Inactive";
					break;
				case "terminated":
					this.lifeCycleStatus = "Terminated";
					break;
			}
			var arrObj = new Object();
			arrObj.id = this.updateSubId;
			arrObj.password = this.updateSubPass;
			arrObj.product = this.updateSubPro;
			arrObj.sessionTimeout = this.updateSubSes;
			arrObj.acctInterimInterval = this.updateSubSessInt;
			arrObj.class = this.updateSubClass;
			arrObj.enabled = this.$.updateSubscriberEnabled.checked
			arrObj.multisession = this.$.updateSubscriberMulti.checked;
			arrObj.product = this.updateSubProduct;
			arrObj.state = this.lifeCycleStatus
			this.valCha.push(arrObj);
			this.$.updateSubscriberModal.open();
		}
	}

	_updateSubscriberProductsResponse(event) {
		var results = event.detail.xhr.response;
		for (var index in results) {
			function checkExist(name) {
				return name == results[index].name;
			}
			if(!this.offers.some(checkExist)){
				this.push('offers', results[index].name);
			}
		}
	}

	_updateSubscriberProductsError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateSubscriberAuthentication(event) {
		var id = this.updateSubId;
		var getAjax = this.$.getServiceRespAjax; 
		var etag = getAjax.lastRequest.xhr.getResponseHeader('ETag');
		var results = getAjax.lastResponse;
		var editAjax =  this.$.updateSubscriberAuthenticationAjax;
		editAjax.method = "PATCH";
		editAjax.headers['If-Match'] = etag;
		editAjax.contentType = "application/json-patch+json";
		var id = this.updateSubId; 
		editAjax.url = "/serviceInventoryManagement/v2/service/" + id;
		for(var indexx in this.valCha) {
			var penAuthSub = new Set();
			var penAuthSubObj = new Object();
			var penAuthSubObj1 = new Object(); 
			if(this.updateSubNPass != this.valCha[indexx].password) {
				penAuthSubObj.password = this.updateSubNPass;
				penAuthSub.add(this.updateSubNPass);
			}
			if (penAuthSub.has(this.updateSubNPass)) {
				penAuthSubObj1.password = this.updateSubNPass;
			}
			this.valChaOne.push(penAuthSubObj1);
		}
		for(var indexx1 in this.valChaOne) {
			var servicePassArr = new Array();
			if(this.valChaOne[indexx1].password) {
				function checkPass(pass) {
					return pass.name == "servicePassword";
				}
				var index = results.serviceCharacteristic.findIndex(checkPass);
				var sub = new Object();
				sub.op = "replace";
				sub.path = "/serviceCharacteristic/" + index;
				var servicePass = new Object();
				servicePass.name = "servicePassword";
				servicePass.value = this.valChaOne[indexx1].password; 
				sub.value = servicePass;
				servicePassArr.push(sub);
			}
		}
		editAjax.body = JSON.stringify(servicePassArr);
		editAjax.generateRequest();
	}

	_updateSubscriberAuthenticationResponse(event) {
		this.updateSubNPass = "";
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_updateSubscriberAuthenticationError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateSubscriberBalance(event) {
		var ajaxBucket = this.$.addBucketAjax;
		var bucketTop = {type: "buckettype"};
		var bunits;
		var bamount;
		if(this.updateSubAmo) {
			if(this.$.updateUni1.selected == 0) {
				bunits = "octets";
			} else if (this.$.updateUni1.selected == 1) {
				bunits = "cents";
			} else if(this.$.updateUni1.selected == 2) {
				bunits = "seconds";
			} else {
				bunits = "cents";
			}
			if(bunits && this.updateSubAmo) {
				var size = this.updateSubAmo;
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
					bamount = this.updateSubAmo;
				} else if(bunits == "seconds") {
					var n = Number(s);
					if(m == "m") {
						n = n * 60;
						bamount = n.toString() + "s";
					} else if(m == "h") {
						n = n * 3600;
						bamount = n.toString() + "s";
					} else if(m == "d") {
						n = n * 86400;
						bamount = n.toString() + "s";
					} else {
						bamount = n.toString() + "s";
					}
				}
				bucketTop.amount = {units: bunits, amount: bamount};
			}
			bucketTop.product = {id: this.updateSubPro,
					href: "/productInventoryManagement/v2/product/" + this.updateSubPro};
			ajaxBucket.headers['Content-type'] = "application/json";
			ajaxBucket.body = bucketTop;
			ajaxBucket.url="/balanceManagement/v1/balanceAdjustment";
			ajaxBucket.generateRequest();
			this.$.updateUni1.selected = null;
			this.updateSubAmo = null;
		}
	}

	_updateSubscriberBalanceResponse(event) {
		document.getElementById("getSubscriberAjax").generateRequest();
	}

	_updateSubscriberBalanceError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateSubscriberAuthorization(event) {
		var getAjax = this.$.getServiceRespAjax;
		var results = getAjax.lastResponse;
		var editAjax =  this.$.updateSubscriberAuthorizationAjax;
		editAjax.method = "PATCH";
		editAjax.contentType = "application/json-patch+json";
		editAjax.url = "/serviceInventoryManagement/v2/service/" + this.updateSubId;
		for(var indexx in this.valCha) {
			var penAuthoSub = new Set();
			var penAuthoSubObj = new Object();
			var penAuthoSubObj1 = new Object(); 
			if(this.$.updateSubscriberEnabled.checked != this.valCha[indexx].enabled) {
				penAuthoSubObj.enabled = this.$.updateSubscriberEnabled.checked;
				penAuthoSub.add(this.$.updateSubscriberEnabled.checked);
			}
			if(this.updateSubSes != this.valCha[indexx].sessionTimeout) {
				var sessionTime;
				var size = this.updateSubSes;
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
					sessionTime = n.toString();
				} else if(m == "h") {
					n = n * 3600;
					sessionTime = n.toString();
				} else if(m == "d") {
					n = n * 86400;
					sessionTime = n.toString();
				} else {
					sessionTime = n.toString();
				}
				this.updateSubSes = sessionTime;
				penAuthoSubObj.sessionTimeout = this.updateSubSes;
				penAuthoSub.add(this.updateSubSes);
			}
			if(this.updateSubSessInt != this.valCha[indexx].acctInterimInterval) {
				var sessionInt;
				var size = this.updateSubSessInt;
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
				this.updateSubSessInt = sessionInt;
				penAuthoSubObj.acctInterimInterval = this.updateSubSessInt;
				penAuthoSub.add(this.updateSubSessInt);
			}
			if(this.$.updateSubscriberMulti.checked != this.valCha[indexx].multisession) {
				penAuthoSubObj.multisession = this.$.updateSubscriberMulti.checked;
				penAuthoSub.add(this.$.updateSubscriberMulti.checked);
			}
			if (penAuthoSub.has(this.$.updateSubscriberEnabled.checked)) {
				penAuthoSubObj1.enabled = this.$.updateSubscriberEnabled.checked;
			}
			if (penAuthoSub.has(this.updateSubSes)) {
				penAuthoSubObj1.sessionTimeout = this.updateSubSes; 
			}
			if (penAuthoSub.has(this.updateSubSessInt)) {
				penAuthoSubObj1.acctInterimInterval = this.updateSubSessInt; 
			}
			if (penAuthoSub.has(this.$.updateSubscriberMulti.checked)) {
				penAuthoSubObj1.multisession = this.$.updateSubscriberMulti.checked; 
			}
			this.valChaOne.push(penAuthoSubObj1);
		}
		for(var indexx1 in this.valChaOne) {
			var serviceAuthoSubArr = new Array();
			if(this.valChaOne[indexx1].enabled != null) {
				var ena = new Object();
				ena.op = "replace";
				ena.path = "/isServiceEnabled";
				ena.value = this.valChaOne[indexx1].enabled;
				serviceAuthoSubArr.push(ena);
			}
			if(this.valChaOne[indexx1].sessionTimeout) {
				function checkTimeout(sessionTime) {
					return sessionTime.name == "sessionTimeout";
				}
				var indexSession = results.serviceCharacteristic.findIndex(checkTimeout);
				if(indexSession == -1) {
					var sub1 = new Object();
					sub1.op = "add";
					sub1.path = "/serviceCharacteristic/-";
					var sessionTimeout = new Object();
					sessionTimeout.name = "sessionTimeout";
					sessionTimeout.value = parseInt(this.valChaOne[indexx1].sessionTimeout);
					sub1.value = sessionTimeout;
					serviceAuthoSubArr.push(sub1);
				} else {
					var sub11 = new Object();
					sub11.op = "replace";
					sub11.path = "/serviceCharacteristic/" + indexSession;
					var sessionTimeout = new Object();
					sessionTimeout.name = "sessionTimeout";
					sessionTimeout.value = parseInt(this.valChaOne[indexx1].sessionTimeout);
					sub11.value = sessionTimeout;
					serviceAuthoSubArr.push(sub11);
				}
			}
			if(this.valChaOne[indexx1].acctInterimInterval) {
				function checkSessionInt(sessionInterval) {
					return sessionInterval.name == "acctSessionInterval";
				}
				var indexSessionInt = results.serviceCharacteristic.findIndex(checkSessionInt);
				if(indexSessionInt == -1) {
					var sub2 = new Object();
					sub2.op = "add";
					sub2.path = "/serviceCharacteristic/-";
					var acctSessionInterval = new Object();
					acctSessionInterval.name = "acctSessionInterval";
					acctSessionInterval.value = parseInt(this.valChaOne[indexx1].acctInterimInterval);
					sub2.value = acctSessionInterval;
					serviceAuthoSubArr.push(sub2);
				} else {
					var sub22 = new Object();
					sub22.op = "replace";
					sub22.path = "/serviceCharacteristic/" + indexSessionInt;
					var acctSessionInterval = new Object();
					acctSessionInterval.name = "acctSessionInterval";
					acctSessionInterval.value = parseInt(this.valChaOne[indexx1].acctInterimInterval);
					sub22.value = acctSessionInterval;
					serviceAuthoSubArr.push(sub22);
				}
			}
			if(this.valChaOne[indexx1].multisession != null) {
				function checkMulti(multiSess) {
					return multiSess.name == "multiSession";
				}
				var indexMulti = results.serviceCharacteristic.findIndex(checkMulti);
				var sub3 = new Object();
				sub3.op = "replace";
				sub3.path = "/serviceCharacteristic/" + indexMulti;
				var multi = new Object();
				multi.name = "multiSession";
				multi.value = this.valChaOne[indexx1].multisession;
				sub3.value = multi;
				serviceAuthoSubArr.push(sub3);
			}
		}
		editAjax.body = JSON.stringify(serviceAuthoSubArr);
		editAjax.generateRequest();
		this.updateSubSessInt = null;
		this.updateSubSes = null;
	}

	_updateSubscriberAuthorizationResponse(event) {
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_updateSubscriberAuthorizationError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	updateSubscriberProduct(event) {
		var getAjax = this.$.getServiceRespAjax;
		var results = getAjax.lastResponse;
		var editAjax =  this.$.updateSubscriberProductAjax;
		editAjax.method = "PATCH";
		editAjax.contentType = "application/json-patch+json";
		editAjax.url = "/serviceInventoryManagement/v2/service/" + this.updateSubId;
		for(var indexx in this.valCha) {
			var penProductSub = new Set();
			var penProductSubObj = new Object();
			var penProductSubObj1 = new Object(); 
			if(this.lifeCycleStatus == "Feasibility Checked") {
				this.lifeCycleStatus = "feasibilityChecked"
			}
			if(this.lifeCycleStatus == "Designed") {
				this.lifeCycleStatus = "designed"
			}
			if(this.lifeCycleStatus == "Reserved") {
				this.lifeCycleStatus = "reserved"
			}
			if(this.lifeCycleStatus == "Active") {
				this.lifeCycleStatus = "active"
			}
			if(this.lifeCycleStatus == "Inactive") {
				this.lifeCycleStatus = "inactive"
			}
			if(this.lifeCycleStatus == "Terminated") {
				this.lifeCycleStatus = "terminated"
			}
			if(this.lifeCycleStatus != this.valCha[indexx].state) {
				penProductSubObj.state = this.lifeCycleStatus;
				penProductSub.add(this.lifeCycleStatus);
			}
			if (penProductSub.has(this.lifeCycleStatus)) {
				penProductSubObj1.state = this.lifeCycleStatus;
			}
			if(this.updateSubProduct != this.valCha[indexx].product) {
				penProductSubObj.product = this.updateSubProduct;
				penProductSub.add(this.updateSubProduct);
			}
			if (penProductSub.has(this.updateSubProduct)) {
				penProductSubObj1.product = this.updateSubProduct;
			}
			this.valChaOne.push(penProductSubObj1);
		}
		for(var indexx1 in this.valChaOne) {
			var serviceProductSubArr = new Array();
			if(this.valChaOne[indexx1].product != null) {
				var pro = new Object();
				pro.op = "add";
				pro.path = "/product";
				pro.value = this.valChaOne[indexx1].product;
				if(pro.value == "") {
					var proDel = new Object();
					proDel.op = "remove";
					proDel.path = "/product";
					serviceProductSubArr.push(proDel);
				} else {
					serviceProductSubArr.push(pro);
				}
			}
			if(this.valChaOne[indexx1].state != null) {
				var stateObj = new Object();
				stateObj.op = "add";
				stateObj.path = "/state"
				stateObj.value = this.valChaOne[indexx1].state;
				if(stateObj.value == "") {
					var stateObjDel = new Object();
					stateObjDel.op = "remove";
					stateObjDel.path = "/state";
					serviceProductSubArr.push(stateObjDel);
				} else {
					serviceProductSubArr.push(stateObj);
				}
			}
		}
		editAjax.body = JSON.stringify(serviceProductSubArr);
		editAjax.generateRequest();
		this.lifeCycleStatus = null;
	}

	_updateSubscriberProductResponse(event) {
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_updateSubscriberProductError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	deleteSubscriber(event) {
		var delSub = this.$.deleteSubscriberAjax;
		delSub.method = "DELETE";
		delSub.url = "/serviceInventoryManagement/v2/service/" + this.updateSubId;
		delSub.generateRequest();
	}

	_deleteSubscriberResponse(event) {
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
		this.$.updateSubscriberModal.close();
	}

	_deleteSubscriberError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	cancelDialog() {
		this.updateSubId = null;
		this.updateSubPass = null;
		this.updateSubPro = null;
		this.updateSubSes = null;
		this.updateSubSessInt = null;
		this.updateSubClass = null;
		this.updateSubNPass = null;
		this.updateSubAmo = null;
		this.$.updateUni1.selected = null;
		this.updateSubSes = null;
		this.updateSubSessInt = null;
		this.updateSubMul = null;
		this.lifeCycleStatus = null;
	}
}

window.customElements.define('sig-sub-update', subUpdate);
