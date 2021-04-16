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
import './style-element.js';

class policyAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="policyAddModal" modal>
				<app-toolbar>
					<div main-title>Add Policy</div>
				</app-toolbar>
				<paper-progress
					indeterminate
					class="slow red"
					disabled="{{!loading}}">
				</paper-progress>
				<paper-input
					label="Name"
					value="{{polName}}">
				</paper-input>
				<div>
					<span>Characteristics</span>
					<paper-icon-button
						id="onClickPolicyChars"
						suffix
						icon="arrow-drop-down"
						on-click="_onClickPolicyChars">
					</paper-icon-button>
				</div>
				<iron-collapse id="addPolicyChars">
					<div>
						<paper-input
							id="bandwidthDL"
							label="maxRequestedBandwidthDL"
							value="{{charDL}}">
						</paper-input>
						<paper-input
							id="bandwidthUL"
							label="maxRequestedBandwidthUL"
							value="{{charUL}}">
						</paper-input>
						<paper-input
							id="classId"
							label="qosClassIdentifier"
							value="{{charClassId}}">
						</paper-input>
						<paper-input
							id="chargeRule"
							label="chargingKey"
							value="{{polCha}}">
						</paper-input>
						<paper-input
							id="flowUp"
							label="flowDirection"
							value="{{flowUp}}">
						</paper-input>
						<paper-input
							id="flowDescription"
							label="flowDescription"
							value="{{flowDiscUp}}">
						</paper-input>
						<paper-input
							id="precedence"
							label="precedence"
							value="{{prece}}">
						</paper-input>
					</div>
				</iron-collapse>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_add">
						Add
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
				id="policyAddAjax"
				content-type="application/json"
				loading="{{loading}}"
				on-response="_response"
				on-error="_error">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			polName: {
				type: String
			},
			charDL: {
				type: Number
			},
			charUL: {
				type: Number
			},
			charClassId: {
				type: Number
			},
			polCha: {
				type: String
			},
			flowUp: {
				type: String 
			},
			flowDiscUp: {
				type: String
			},
			flowDown: {
				type: String
			},
			flowDiscDown: {
				type: String
			},
			prece: {
				type: String
			},
			table: {
				type: String
			},
			tableId: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.$.policyAddModal.close();
		this.polName = null;
		this.charDL = null;
		this.charUL = null;
		this.charClassId = null;
		this.polCha = null;
		this.flowUp = null;
		this.flowDiscUp = null;
		this.flowDown = null;
		this.flowDiscDown = null;
		this.prece = null;
	}

	_onClickPolicyChars() {
		if(this.$.addPolicyChars.opened == false) {
			this.$.addPolicyChars.show();
			this.$.onClickPolicyChars.icon="arrow-drop-up"
		} else {
			this.$.addPolicyChars.hide();
			this.$.onClickPolicyChars.icon="arrow-drop-down"
		}
	}

	_add() {
		var ajax = this.$.policyAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource/";
		var pol = new Object();
		if(this.polName) {
			pol.name = this.polName;
		}
		var rel = new Array();
		var relObj = new Object();
		relObj.id = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').tableId;
		relObj.href = "/resourceInventoryManagement/v1/resourceRelationship/" + relObj.id;
		relObj.name = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').table;
		relObj.type = "contained";
		rel.push(relObj);
		pol.resourceRelationship = rel;

		var charaArr = new Array();
		var nameObj = new Object();
		nameObj.name = "name";
		nameObj.minCardinality = 1;
		nameObj.value = this.polName;
		var nameQOS = new Object();
		nameQOS.name = "qosInformation";
		nameQOS.minCardinality = 0;
		var nameQOSValue = new Object(); 
		nameQOSValue.maxRequestedBandwidthDL = parseInt(this.charDL);
		nameQOSValue.maxRequestedBandwidthUL = parseInt(this.charUL);
		nameQOSValue.qosClassIdentifier = parseInt(this.charClassId);
		nameQOS.value = nameQOSValue;
		var nameChRule = new Object();
		nameChRule.name = "chargingKey";
		nameChRule.minCardinality = 1;
		nameChRule.value = parseInt(this.polCha);
		var nameFlow = new Object();
		nameFlow.name = "flowInformation";
		nameFlow.minCardinality = 1;
		var nameFloArr = new Array();
		var nameFloObj = new Object();
		nameFloObj.flowDirection = this.flowUp;
		nameFloObj.flowDescription = this.flowDiscUp;
		nameFloArr.push(nameFloObj);
		nameFlow.value = nameFloArr;
		var namePre = new Object();
		namePre.name = "precedence";
		namePre.minCardinality = 1;
		namePre.value = parseInt(this.prece);
		charaArr.push(nameObj, nameQOS, nameChRule, nameFlow, namePre);
		pol.resourceCharacteristic = charaArr;

		var spec = new Object();
		spec.id = "4";
		spec.name = "PolicyTable";
		pol.resourceSpecification = spec;
		ajax.body = pol;
		ajax.generateRequest();
		this.$.policyAddModal.close();
	}

	_response() {
		this.$.policyAddModal.close
		this.polName = null;
		this.charDL = null;
		this.charUL = null;
		this.charClassId = null;
		this.polCha = null;
		this.flowUp = null;
		this.flowDiscUp = null;
		this.flowDown = null;
		this.flowDiscDown = null;
		this.prece = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}
}

window.customElements.define('sig-policy-add', policyAdd);
