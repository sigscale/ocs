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
import '@polymer/iron-pages/iron-pages.js';
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class policyUpdate extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="updatePolicyModal" modal>
				<app-toolbar>
					<h2>Update Policy</h2>
				</app-toolbar>
				<paper-progress
						id="progressId"
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id = "policyId"
						label="Id"
						value="{{polId}}"
						disabled = true>
				</paper-input>
				<paper-input
						id = "policyName"
						label="Name"
						value="{{polName}}">
				</paper-input>
				<paper-input
						id = "policyDescription"
						label="Description"
						value="{{polDesc}}">
				</paper-input>
				<paper-input
						id = "policyCategory"
						label="Category"
						value="{{polCat}}">
				</paper-input>
				<paper-input
						id = "policySpecification"
						label="Specification"
						value="{{polSpe}}">
				</paper-input>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_add">
						Update	
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
					<paper-button
							toggles
							raised
							class="delete-button"
							on-tap="deletePolicy">
						Delete
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="policyUpdateAjax"
					loading="{{loading}}"
					on-response="_response"
					on-error="_error">
			</iron-ajax>
			<iron-ajax
					id="deletePolicyAjax"
					on-response="_deletePolicyResponse"
					on-error="_deletePolicyError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			activeItem: {
				type: Object,
				observer: '_activeItemChanged'
			},
			polDesc: {
				type: String
			},
			polCat: {
				type: String
			},
			polName: {
				type: String
			},
			polSpe: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_activeItemChanged(item) {
		if (item != null){
			this.polId = item.id
			this.polName = item.name;
			this.polDesc = item.description;
			this.polCat = item.category;
			this.polSpe = item.resourceSpecification.name;
		} else {
			this.polId = null;
			this.polName = null;
			this.polDesc = null;
			this.polCat = null;
			this.polSpe = null;
		}
	}

   deletePolicy() {
      this.$.deletePolicyAjax.method = "DELETE";
      this.$.deletePolicyAjax.url = "/resourceInventoryManagement/v1/resource/" + document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').selectedItems[0].id;
      this.$.deletePolicyAjax.generateRequest();
   }

	_deletePolicyResponse() {
      this.$.updatePolicyModal.close();
      document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

	_add(event) {
		var Ajax = this.$.policyUpdateAjax;
		Ajax.method = "PATCH"
		Ajax.contentType = "application/json-patch+json";
		Ajax.url = "/resourceInventoryManagement/v1/resource/" + this.polId;
		var PolArray = new Array();
		var Name = new Object();
		Name.op = "add";
		Name.path = "/name";
      Name.value = this.polName;
      PolArray.push(Name);
		var Desc = new Object();
		Desc.op = "add";
		Desc.path = "/description";
      Desc.value = this.polDesc;
      PolArray.push(Desc);
		var Cat = new Object();
		Cat.op = "add";
		Cat.path = "/category";
      Cat.value = this.polCat;
      PolArray.push(Cat);
		var Spe = new Object();
		Spe.op = "add";
		Spe.path = "/resourceSpecification/name";
      Spe.value = this.polSpe;
      PolArray.push(Spe);
		Ajax.body = JSON.stringify(PolArray);
		Ajax.generateRequest();
		this.polName = null;
		this.polDesc = null;
		this.polCat = null;
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-update').shadowRoot.getElementById('updatePolicyModal').close();
	}

	_response(event) {
		document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
	}

   _cancel() {
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-policy-update').shadowRoot.getElementById('updatePolicyModal').close();
		this.polName = null;
		this.polDesc = null;
		this.polCat = null;
   }
}

window.customElements.define('sig-policy-update', policyUpdate);
