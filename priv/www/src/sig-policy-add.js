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
import '@polymer/paper-tooltip/paper-tooltip.js';
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
				<div>
					<paper-input
						id="policyNameAdd"
						label="Name"
						value="{{policyName}}">
					</paper-input>
					<paper-tooltip
							for="policyNameAdd"
							offset="0">
						Policy row name
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="bandwidthDL"
						label="MaxRequestedBandwidthDL"
						value="{{bandwidthDL}}">
					</paper-input>
					<paper-tooltip
							for="bandwidthDL"
							offset="0">
						Max Download Bandwidth
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="bandwidthUL"
						label="MaxRequestedBandwidthUL"
						value="{{bandwidthUL}}">
					</paper-input>
					<paper-tooltip
							for="bandwidthUL"
							offset="0">
						Max Upload Bandwidth
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="classId"
						label="QosClassIdentifier"
						value="{{classID}}">
					</paper-input>
					<paper-tooltip
							for="classId"
							offset="0">
						QoS Class
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="chargingKey"
						label="ChargingKey"
						value="{{chargingKey}}">
					</paper-input>
					<paper-tooltip
							for="chargingKey"
							offset="0">
						Charging Key
					</paper-tooltip>
				</div>
				<div>
					<paper-dropdown-menu
							id="flowDirection"
							value="{{flowDirection}}"
							no-animations="true"
							label="Flow Direction">
						<paper-listbox
								id="addPolDirList1"
								slot="dropdown-content">
							<paper-item>
								up
							</paper-item>
							<paper-item>
								down
							</paper-item>
						</paper-listbox>
					</paper-dropdown-menu>
					<paper-tooltip
							for="flowDirection"
							offset="0">
						Flow Direction
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="flowDescription"
						label="FlowDescription"
						pattern="^permit out proto "
						placeholder="permit out proto ip from all to all"
						value="{{flowDescription}}"
						auto-validate>
					</paper-input>
					<paper-tooltip
							for="flowDescription"
							offset="0">
						Flow Key
					</paper-tooltip>
				</div>
				<div>
					<paper-input
						id="precedence"
						label="Precedence"
						value="{{precedence}}">
					</paper-input>
					<paper-tooltip
							for="precedence"
							offset="0">
						Precedence value
					</paper-tooltip>
				</div>
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
			policyName: {
				type: String
			},
			bandwidthDL: {
				type: Number
			},
			bandwidthUL: {
				type: Number
			},
			classID: {
				type: Number
			},
			chargingKey: {
				type: String
			},
			flowDirection: {
				type: String
			},
			flowDescription: {
				type: String
			},
			precedence: {
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
		this.policyName = null;
		this.bandwidthDL = null;
		this.bandwidthUL = null;
		this.classID = null;
		this.chargingKey = null;
		this.flowDirection = null;
		this.flowDescription = null;
		this.precedence = null;
	}

	_add() {
		var ajax = this.$.policyAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource/";
		var policy = new Object();
		if(this.policyName) {
			policy.name = this.policyName;
		}
		var relationships = new Array();
		var related = new Object();
		related.id = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').tableId;
		related.href = "/resourceInventoryManagement/v1/resourceRelationship/" + related.id;
		related.name = document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-list').table;
		var relationship = new Object();
		relationship.relationshipType = "contained";
		relationship.resource = related;
		relationships.push(relationship);
		policy.resourceRelationship = relationships;
		var characteristics = new Array();
		var char1 = new Object();
		char1.name = "name";
		char1.value = this.policyName;
		var char2 = new Object();
		char2.name = "qosInformation";
		var qos = new Object();
		qos.maxRequestedBandwidthDL = parseInt(this.bandwidthDL);
		qos.maxRequestedBandwidthUL = parseInt(this.bandwidthUL);
		qos.qosClassIdentifier = parseInt(this.classID);
		char2.value = qos;
		var char3 = new Object();
		char3.name = "chargingKey";
		char3.value = parseInt(this.chargingKey);
		var char4 = new Object();
		char4.name = "flowInformation";
		var flows = new Array();
		var flow = new Object();
		flow.flowDirection = this.flowDirection;
		if(this.flowDescription) {
			flow.flowDescription = this.flowDescription;
		} else {
			flow.flowDescription = "permit out proto ip from all to all"
		}
		flows.push(flow);
		char4.value = flows;
		var char5 = new Object();
		char5.name = "precedence";
		char5.value = parseInt(this.precedence);
		characteristics.push(char1, char2, char3, char4, char5);
		policy.resourceCharacteristic = characteristics;
		var specification = new Object();
		specification.id = "4";
		specification.name = "PolicyTable";
		specification.href = "resourceCatalogManagement/v2/resourceSpecification/" + "4";
		policy.resourceSpecification = specification;
		ajax.body = policy;
		ajax.generateRequest();
		this.$.policyAddModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

	_response() {
		this.$.policyAddModal.close
		this.policyName = null;
		this.bandwidthDL = null;
		this.bandwidthUL = null;
		this.classID = null;
		this.chargingKey = null;
		this.flowDirection = null;
		this.flowDescription = null;
		this.precedence = null;
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Success";
		toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

	_error(){
      var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
      toast.text = "Error";
      toast.open();
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}
}

window.customElements.define('sig-policy-add', policyAdd);
