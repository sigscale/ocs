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

class prefixAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog class="dialog" id="addPrefixModal" modal>
				<app-toolbar>
					<h2>Add Prefix</h2>
				</app-toolbar>
				<paper-progress
						indeterminate
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<paper-input
						id="addPrefix"
						type="number"
						name="Prefix"
						allowed-pattern="[0-9]"
						label="Prefix"
						value="{{addPre}}">
				</paper-input>
				<paper-input
						id="addDesc"
						name="Description"
						label="Description"
						value="{{addPreDesc}}">
				</paper-input>
				<paper-input
						id="addRateRow"
						type="number"
						name="PriceName"
						label="Rate"
						value="{{addPreRate}}">
				</paper-input>
				<div class="buttons">
					<paper-button dialog-confirm
							raised
							on-tap="_tableRow"
							class="add-button">
						Submit
					</paper-button>
					<paper-button
							class="cancel-button"
							on-tap="_cancel"
							dialog-dismiss>
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<paper-toast
					id="addTableRowToastError">
			</paper-toast>
			<iron-ajax
					id="addTableRowAjax"
					url="/catalogManagement/v2/pla"
					method = "POST"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addTableResponse"
					on-error="_addTableError">
			</iron-ajax>
		`;
	}
	static get properties() {
		return {
				loading: {
				type: Boolean,
				value: false
			},
			addPre: {
				type: String,
			},
			addPreDesc: {
				type: String,
			},
			addPreRate: {
				type: String,
			}
		}
	}

	_tableRow(event) {
//		var table = document.getElementById('prefixList').table;
		var table = document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList');
		var rowName = new Object();
		rowName.id = this.addPre;
		rowName.href = "/resourceInventoryManagement/v1/logicalResource/" + table + "/" + rowName.id;
		var resource = new Array();
		var resPre = new Object();
		resPre.name = "prefix";
		seqNum = 1;
		value = this.addPre;
		resPre.value = {seqNum, value};
		resource.push(resPre);
		var resDes = new Object();
		resDes.name = "description";
		seqNum = 2;
		value = this.addPreDesc;
		resDes.value = {seqNum, value};
		resource.push(resDes);
		var resRate = new Object();
		resRate.name = "rate";
		seqNum = 3;
		value = this.addPreRate;
		resRate.value = {seqNum, value};
		resource.push(resRate);
		rowName.resourceCharacteristic = resource;
		var ajax = this.$.addTableRowAjax;
		ajax.url = "/resourceInventoryManagement/v1/logicalResource/" + table
		ajax.body = rowName;
		ajax.generateRequest();
		this.addPre = null;
		this.addPreDesc = null;
		this.$.addRateRow.value = null;
	}

	_addTableResponse(event) {
		this.$.addPrefixModal.close();
		document.body.querySelector('sig-app').shadowRoot.getElementById('prefixList').shadowRoot.getElementById('prefixGrid').clearCache();
	}

	_addTableError(event) {
		this.$.addTableRowToastError.text = event.detail.request.xhr.statusText;
		this.$.addTableRowToastError.open();
	}

	_cancel() {
		this.addPre = null;
		this.addDesc = null;
		this.addRateRow = null;
	}
}

window.customElements.define('sig-prefix-add', prefixAdd);
