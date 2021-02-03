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
				<paper-input
					label="Description"
					value="{{polDescription}}">
				</paper-input>
				<paper-input
					label="Version"
					value="{{polVersion}}">
				</paper-input>
				<paper-input
					label="Type"
					value="{{polType}}">
				</paper-input>
				<paper-input
					label="Status"
					value="{{polStatus}}">
				</paper-input>
				<paper-input
					label="Category"
					value="{{polCategory}}">
				</paper-input>
				<paper-input
					label="Specification"
					value="{{polSpec}}">
				</paper-input>
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
			polDescription: {
				type: String
			},
			polVersion: {
				type: String
			},
			polCategory: {
				type: String
			},
			polType: {
				type: String
			},
			polStatus: {
				type: String
			},
			polSpec: {
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
		this.polDescription = null;
		this.polVersion = null;
		this.polCategory = null;
		this.polType = null;
		this.polStatus = null;
		this.polSpec = null;
	}

	_add() {
		var ajax = this.$.policyAddAjax;
		ajax.method = "POST";
		ajax.url = "/resourceInventoryManagement/v1/resource/";
      var pol = new Object();
      if(this.polName) {
         pol.name = this.polName;
      }
      if(this.polDescription) {
         pol.description = this.polDescription;
      }
      if(this.polVersion) {
         pol.version = this.polVersion;
      }
      if(this.polType) {
         pol['@type'] = this.polType;
      }
      if(this.polStatus) {
         pol.lifecycleStatus = this.polStatus;
      }
      if(this.polCategory) {
         pol.category = this.polCategory;
      }
      if(this.polSpec) {
			var spec = new Object();
			spec.id = "4";
			spec.href = "/resourceInventoryManagement/v1/resource/vwqmumpshq" + spec.id;
			spec.name = this.polSpec;
         pol.resourceSpecification = spec;
      }
      ajax.body = JSON.stringify(pol);
      ajax.generateRequest();
		this.$.policyAddModal.close();
	}

   _response() {
		this.$.policyAddModal.close
      this.polName = null;
      this.polDescription = null;
      this.polType = null;
      this.polStatus = null;
      this.polVersion = null;
      this.polCategory = null;
      this.polSpec = null;
      document.body.querySelector('sig-app').shadowRoot.getElementById('policyList').shadowRoot.getElementById('policyGrid').clearCache();
   }
}

window.customElements.define('sig-policy-add', policyAdd);
