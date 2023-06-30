/**
 * Copyright 2016 - 2023 SigScale Global Inc.
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
import '@polymer/paper-toggle-button/paper-toggle-button';
import '@polymer/paper-button/paper-button.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class policyAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="policyAddModal"
					modal>
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
							label="Policy Rule Name"
							value="{{policyName}}">
					</paper-input>
					<paper-tooltip>
						Uniquely identifies a PCRF provided rule within a session, or references a rule predefined at PCEF.
					</paper-tooltip>
				</div>
				<div>
					<paper-toggle-button
							checked="{{predefined}}">
						Predefined
					</paper-toggle-button>
					<paper-tooltip>
						Indicates if the rule is predefined at the PCEF.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Max Bandwidth DL"
							value="{{bandwidthDL}}">
					</paper-input>
					<paper-tooltip>
						Maximum allowed bit rate for downlink.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Max Bandwidth UL"
							value="{{bandwidthUL}}">
					</paper-input>
					<paper-tooltip>
						Maximum allowed bit rate for uplink.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Qos Class Identifier (QCI)"
							value="{{classID}}">
					</paper-input>
					<paper-tooltip>
						Identifies the authorized QoS for the default EPS bearer.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Charging Key"
							value="{{chargingKey}}">
					</paper-input>
					<paper-tooltip>
						Charging Key (Rating Group) used for rating the service associated with the service flow of this rule.
					</paper-tooltip>
				</div>
				<div>
					<paper-dropdown-menu
							value="{{flowDirection}}"
							no-animations="true"
							label="Flow Direction">
						<paper-listbox
								slot="dropdown-content">
							<paper-item>
								up
							</paper-item>
							<paper-item>
								down
							</paper-item>
							<paper-item>
								both
							</paper-item>
						</paper-listbox>
					</paper-dropdown-menu>
					<paper-tooltip>
						Indicates the direction that a filter is applicable: downlink only, uplink only or both (bidirectional).
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Flow Filter"
							pattern="^permit out proto .+"
							placeholder="permit out proto ip from all to all"
							value="{{flowDescription}}"
							auto-validate>
					</paper-input>
					<paper-tooltip>
						Defines a packet filter for an IP flow (e.g. permit out proto ip from all to all).
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							label="Precedence"
							value="{{precedence}}">
					</paper-input>
					<paper-tooltip>
						Determines the order in which service data flow templates are applied for detection at the PCEF. A rule with a lower value shall be applied before a rule with a higher value.
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
					id="policyAdd"
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
			predefined: {
				type: Boolean,
				value: false
			},
			bandwidthDL: {
				type: String 
			},
			bandwidthUL: {
				type: String 
			},
			classID: {
				type: String
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
			}
		}
	}

	ready() {
		super.ready()
	}

	_cancel() {
		this.$.policyAddModal.close();
		this.policyName = null;
		this.predefined = false;
		this.bandwidthDL = null;
		this.bandwidthUL = null;
		this.classID = null;
		this.chargingKey = null;
		this.flowDirection = null;
		this.flowDescription = null;
		this.precedence = null;
	}

	_add() {
		var policyList = document.body.querySelector('sig-app').shadowRoot.getElementById('policyList');
		if(policyList.activeTableId) {
			var ajax = this.$.policyAdd;
			ajax.method = "POST";
			ajax.url = "/resourceInventoryManagement/v1/resource/";
			var policy = new Object();
			if(this.policyName) {
				policy.name = this.policyName;
			}
			var relationships = new Array();
			var related = new Object();
			related.id = policyList.activeTableId;
			related.href = "/resourceInventoryManagement/v1/resourceRelationship/" + related.id;
			related.name = policyList.activeTableName;
			var relationship = new Object();
			relationship.relationshipType = "contained";
			relationship.resource = related;
			relationships.push(relationship);
			policy.resourceRelationship = relationships;
			var characteristics = new Array();
			var char1 = new Object();
			char1.name = "name";
			char1.value = this.policyName;
			characteristics.push(char1);
			if(this.bandwidthDL || this.bandwidthUL) {
				var char2 = new Object();
				char2.name = "qosInformation";
				var qos = new Object();
					if(this.bandwidthDL) {
						qos.maxRequestedBandwidthDL = parseInt(this.bandwidthDL);
					}
					if(this.bandwidthUL) {
						qos.maxRequestedBandwidthUL = parseInt(this.bandwidthUL);
					}
					if(this.classID) {
						qos.qosClassIdentifier = parseInt(this.classID);
					}
				char2.value = qos;
				characteristics.push(char2);
			}
			if(this.chargingKey) {
				var char3 = new Object();
				char3.name = "chargingKey";
				char3.value = parseInt(this.chargingKey);
				characteristics.push(char3);
			}
			if(this.flowDirection || this.flowDescription) {
				var char4 = new Object();
				char4.name = "flowInformation";
				var flows = new Array();
				var flow = new Object();
				if(this.flowDirection) {
					flow.flowDirection = this.flowDirection;
				}
				if(this.flowDescription) {
					flow.flowDescription = this.flowDescription;
				} else {
					flow.flowDescription = "permit out proto ip from all to all" ;
				}
				flows.push(flow);
				char4.value = flows;
				characteristics.push(char4);
			}
			if(this.precedence) {
				var char5 = new Object();
				char5.name = "precedence";
				char5.value = parseInt(this.precedence);
				characteristics.push(char5);
			}
			if(this.predefined) {
				var char6 = new Object();
				char6.name = "predefined";
				char6.value = this.predefined;
				characteristics.push(char6);
			}
			policy.resourceCharacteristic = characteristics;
			var specification = new Object();
			specification.id = "4";
			specification.href = "/resourceCatalogManagement/v2/resourceSpecification/4";
			specification.name = "PolicyTableRow";
			policy.resourceSpecification = specification;
			ajax.body = policy;
			ajax.generateRequest();
			this.$.policyAddModal.close();
			policyList.shadowRoot.getElementById('policyGrid').clearCache();
		}
	}

	_response() {
		this.$.policyAddModal.close
		this.policyName = null;
		this.predefined = false;
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
