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
					label="Category"
					value="{{polCategory}}">
				</paper-input>
				<paper-input
					label="Type"
					value="{{polType}}">
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
			polCategory: {
				type: String
			},
			polType: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}
}

window.customElements.define('sig-policy-add', policyAdd);
