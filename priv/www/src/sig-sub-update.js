/**
 * @license
 * Copyright (c) 2020 The Polymer Project Authors. All rights reserved.
 * This code may only be used under the BSD style license found at http://polymer.github.io/LICENSE.txt
 * The complete set of authors may be found at http://polymer.github.io/AUTHORS.txt
 * The complete set of contributors may be found at http://polymer.github.io/CONTRIBUTORS.txt
 * Code distributed by Google as part of the polymer project is also
 * subject to an additional IP rights grant found at http://polymer.github.io/PATENTS.txt
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
import '@polymer/paper-toast/paper-toast.js';
import '@polymer/paper-toggle-button/paper-toggle-button.js';
import './style-element.js';

class subUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updateSubscriberModal" modal>
				<app-toolbar>
					<paper-tabs selected="{{selected}}">
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
						<div class="buttons">
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
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
						</div>
					</div>
					<div id="edit-attributes">
						<paper-input
								id="updateSubscriberTimeout"
								name="sessionTimeout"
								value="{{updateSubSes}}"
								type="number"
								label="Session Timeout">
						</paper-input>
						<paper-input
								id="updateSubscriberInterval"
								name="acctSessionInterval"
								value="{{updateSubSessInt}}"
								type="number"
								label="Accounting Interval">
						</paper-input>
						<div>
							<paper-input
									id="updateSubscriberClass"
									name="class"
									value="{{updateSubClass}}"
									type="text"
									label="Class">
							</paper-input>
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
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberAuthorization"
									class="update-button">
								Update
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="deleteSub"
									class="delete-button">
								Delete
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
								type="number"
								label="Amount">
						</paper-input>
						<div>
							<paper-dropdown-menu
									id="updateUni"
									value="{{updateSubUni}}"
									label="Units">
								<paper-listbox
										id="updateUni1"
										slot="dropdown-content"
										selected="0">
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
						</div>
						<div class="buttons">
							<paper-button dialog-dismiss
									on-tap="cancelDialog"
									class="cancel-button">
								Cancel
							</paper-button>
							<paper-button dialog-confirm
									autofocus
									on-tap="updateSubscriberBalance"
									class="update-button">
								Add
							</paper-button>
							<paper-button
									toggles
									raised
									on-tap="deleteSub"
									class="delete-button">
								Delete
							</paper-button>
						</div>
					</div>
				</iron-pages>
				<paper-toast
						id="updateSubscriberToastError">
				</paper-toast>
			</paper-dialog>
			<iron-ajax
					id="updateSubscriberAuthenticationAjax"
					on-response="_updateSubscriberAuthenticationResponse"
					on-error="_updateSubscriberAuthenticationError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberAuthorizationAjax"
					on-response="_updateSubscriberAuthorizationResponse"
					on-error="_updateSubscriberAuthorizationError">
			</iron-ajax>
			<iron-ajax
					id="deleteSubscriberAjax"
					on-response="_deleteSubscriberResponse"
					on-error="_deleteSubscriberError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberBalance"
					content-type="application/json-patch+json"
					method="PATCH"
					on-response="_updateSubscriberBalanceResponse"
					on-error="_updateSubscriberBalanceError">
			</iron-ajax>
			<iron-ajax
					id="getServiceRespAjax"
					method = "GET"
					on-error="_getServiceError">
			</iron-ajax>
			<iron-ajax
					id="updateSubscriberProductsAjax"
					url="/catalogManagement/v2/productOffering"
					method = "GET"
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

	_activeItemChanged(item) {
		if (item != null){
			var ajaxCh = this.$.getServiceRespAjax;
			ajaxCh.url = "/serviceInventoryManagement/v2/service/" + item.id;
			ajaxCh.generateRequest();
			this.$.updateSubscriberProductsAjax.generateRequest();
			this.updateSubId = item.id;
			this.updateSubPass = item.password;
			this.updateSubPro = item.product;
			this.updateSubSes = item.sessionTimeout;
			this.updateSubSessInt = item.acctInterimInterval;
			this.updateSubClass =  item.class;
			this.$.updateSubscriberEnabled.checked =  item.enabled;
			this.$.updateSubscriberMulti.checked =  item.multisession;
			this.$.updateSubscriberModal.open();
		} else {
			this.updateSubId = null;
			this.updateSubPass = null;
			this.updateSubPro = null;
			this.updateSubSes = null;
			this.updateSubSessInt = null;
			this.updateSubClass = null;
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
		this.$.updateSubscriberToastError.text = event.detail.request.xhr.statusText;
		this.$.updateSubscriberToastError.open();
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
		function checkPass(pass) {
			return pass.name == "servicePassword";
		}
		var index = results.serviceCharacteristic.findIndex(checkPass);
		var sub = new Object();
		sub.op = "replace";
		sub.path = "/serviceCharacteristic/" + index;
		var servicePass = new Object();
		servicePass.name = "servicePassword";
		servicePass.value = this.updateSubNPass; 
		sub.value = servicePass;
		editAjax.body = JSON.stringify([sub]);
		editAjax.generateRequest();
	}

	_updateSubscriberAuthenticationResponse(event) {
		this.$.updateSubscriberNewPassword.value = "";
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_updateSubscriberAuthenticationError(event) {
		this.$.updateSubscriberToastError.text = event.detail.request.xhr.statusText;
		this.$.updateSubscriberToastError.open();
	}

	updateSubscriberBalance(event) {
		var editAjax =  this.$.updateSubscriberBalance;
		editAjax.url = "/ocs/v1/subscriber/" + this.updateSubId;
		var sub = new Object();
		sub.op = "add";
		sub.path = "/buckets/-";
		var totalBal;
		if(this.updateSubUni == "Bytes"){
			totalBal = {"remainAmount":parseInt(this.updateSubAmo)};
			totalBal.units = "octets";
		}
		if(this.updateSubUni == "cents"){
			totalBal = {"remainAmount":parseInt(this.updateSubAmo)};
			totalBal.units = "cents";
		}
		if(this.updateSubUni == "seconds"){
			totalBal = {"remainAmount":parseInt(this.updateSubAmo)};
			totalBal.units = "seconds";
		}
		//totalBal.product = document.getElementById("updatePro").value;
		sub.value = totalBal;
		editAjax.body = JSON.stringify([sub]);
		editAjax.generateRequest();
	}

	_updateSubscriberBalanceResponse(event) {
		document.getElementById("getSubscriberAjax").generateRequest();
	}

	_updateSubscriberBalanceError(event) {
		this.$.updateSubscriberToastError.text = event.detail.request.xhr.statusText;
		this.$.updateSubscriberToastError.open();
	}

	updateSubscriberAuthorization(event) {
		var getAjax = this.$.getServiceRespAjax;
		var results = getAjax.lastResponse;
		var editAjax =  this.$.updateSubscriberAuthorizationAjax;
		editAjax.method = "PATCH";
		editAjax.contentType = "application/json-patch+json";
		editAjax.url = "/serviceInventoryManagement/v2/service/" + this.updateSubId;
		var patch = new Array();
		var ena = new Object();
		ena.op = "replace";
		ena.path = "/isServiceEnabled";
		ena.value = this.$.updateSubscriberEnabled.checked;
		patch.push(ena);
		if(this.updateSubSes) {
			function checkTimeout(sessionTime) {
				return sessionTime.name == "sessionTimeout";
			}
			var indexSession = results.serviceCharacteristic.findIndex(checkTimeout);
			var sub = new Object();
			sub.op = "replace";
			sub.path = "/serviceCharacteristic/" + indexSession;
			var sessionTimeout = new Object();
			sessionTimeout.name = "sessionTimeout";
			sessionTimeout.value = parseInt(this.updateSubSes);
			sub.value = sessionTimeout;
			patch.push(sub);
		} else {
			function checkTimeout(sessionTime) {
				return sessionTime.name == "sessionTimeout";
			}
			var indexSession = results.serviceCharacteristic.findIndex(checkTimeout);
			if(indexSession != -1) {
				var sub = new Object();
				sub.op = "replace";
				sub.path = "/serviceCharacteristic/" + indexSession;
				var sessionTimeout = new Object();
				sessionTimeout.name = "sessionTimeout";
				sessionTimeout.value = results.serviceCharacteristic[indexSession].value;
				sub.value = sessionTimeout;
				patch.push(sub);
			}
		}
		if(this.updateSubSessInt) {
			function checkSessionInt(sessionInterval) {
				return sessionInterval.name == "acctSessionInterval";
			}
			var indexSessionInt = results.serviceCharacteristic.findIndex(checkSessionInt);
			var sub1 = new Object();
			sub1.op = "replace";
			sub1.path = "/serviceCharacteristic/" + indexSessionInt;
			var acctSessionInterval = new Object();
			acctSessionInterval.name = "acctSessionInterval";
			acctSessionInterval.value = parseInt(this.updateSubSessInt);
			sub1.value = acctSessionInterval;
			patch.push(sub1);
		} else{
			function checkSessionInt(sessionInterval) {
				return sessionInterval.name == "acctSessionInterval";
			}
			var indexSessionInt = results.serviceCharacteristic.findIndex(checkSessionInt);
			if(indexSessionInt != -1) {
				var sub1 = new Object();
				sub1.op = "replace";
				sub1.path = "/serviceCharacteristic/" + indexSessionInt;
				var acctSessionInterval = new Object();
				acctSessionInterval.name = "acctSessionInterval";
				acctSessionInterval.value = results.serviceCharacteristic[indexSessionInt].value;
				sub1.value = acctSessionInterval;
				patch.push(sub1);
			}
		}
		function checkMulti(multiSess) {
			return multiSess.name == "multiSession";
		}
		var indexMulti = results.serviceCharacteristic.findIndex(checkMulti);
		var sub2 = new Object();
		sub2.op = "replace";
		sub2.path = "/serviceCharacteristic/" + indexMulti;
		var multi = new Object();
		multi.name = "multiSession";
		multi.value = this.updateSubMul;
		sub2.value = multi;
		patch.push(sub2);
		editAjax.body = JSON.stringify(patch);
		editAjax.generateRequest();
	}

	_updateSubscriberAuthorizationResponse(event) {
		document.body.querySelector('sig-app').shadowRoot.getElementById('serviceList').shadowRoot.getElementById('subscriberGrid').clearCache();
	}

	_updateSubscriberAuthorizationError(event) {
		this.$.updateSubscriberToastError.text = event.detail.request.xhr.statusText;
		this.$.updateSubscriberToastError.open();
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
		this.$.updateSubscriberToastError.text = event.detail.request.xhr.statusText;
		this.$.updateSubscriberToastError.open();
	}

	cancelDialog() {
		this.updateSubId = null;
		this.updateSubPass = null;
		this.updateSubPro = null;
		this.updateSubSes = null;
		this.updateSubSessInt = null;
		this.updateSubClass = null;
		this.updateSubNPass = null;
		this.updateSubUni = null;
		this.updateSubSes = null;
		this.updateSubSessInt = null;
		this.updateSubMul = null;
	}
}

window.customElements.define('sig-sub-update', subUpdate);
