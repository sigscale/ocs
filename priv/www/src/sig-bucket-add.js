/**
 * Copyright 2016 - 2021 SigScale Global Inc.
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
import '@polymer/paper-tooltip/paper-tooltip.js';
import './style-element.js';

class offerAdd extends PolymerElement {
	static get template() {
		return html`
			<style include="style-element"></style>
			<paper-dialog
					class="dialog"
					id="addBucketModal"
					modal>
				<app-toolbar>
					<h3>Add Bucket</h3>
				</app-toolbar>
				<paper-progress
						indeterminate
						id="progressId"
						class="slow red"
						disabled="{{!loading}}">
				</paper-progress>
				<div>
					<paper-input
							value="{{lifeStatus}}"
							label="Lifecyclestatus">
					</paper-input>
				</div>
				<div>
					<paper-input
							value="{{productId}}"
							label="Product">
					</paper-input>
					<paper-tooltip>
						Identifier of the Product associated with this bucket.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							type="datetime-local"
							value="{{bucketStart}}"
							label="Start Date">
					</paper-input>
					<paper-tooltip>
						Start of balance bucket validity period.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							type="datetime-local"
							value="{{bucketEnd}}"
							label="End Date">
					</paper-input>
					<paper-tooltip>
						End of balance bucket validity period.
					</paper-tooltip>
				</div>
				<div>
					<paper-dropdown-menu
							value="{{bucketUnit}}"
							no-animations="true"
							label="Units">
						<paper-listbox
								id="addBucket"
								slot="dropdown-content">
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
					<paper-tooltip>
						Type of units contained in this bucket.
					</paper-tooltip>
				</div>
				<div>
					<paper-input
							type="text"
							allowed-pattern="[0-9kmg]"
							pattern="^[0-9]+[kmg]?$"
							label="Amount"
							value="{{bucketAmount}}"
							auto-validate>
					</paper-input>
					<paper-tooltip>
						Total amount of units contained in this bucket.
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button
							raised
							class="submit-button"
							on-tap="_bucketAddSubmit">
						Add
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							on-tap="_cancel">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<paper-dialog
					class="dialog"
					id="deleteBucketModal"
					modal>
				<app-toolbar>
					<h3>Delete Bucket</h3>
				</app-toolbar>
				<div>
					<paper-input
							value="{{deleteBucketId}}"
							label="Bucket Id"
							required
							disabled>
					</paper-input>
					<paper-tooltip>
						Identifier of bucket to be deleted.
					</paper-tooltip>
				</div>
				<div class="buttons">
					<paper-button raised
							on-tap="_deleteBucket"
							class="delete-button">
						Delete
					</paper-button>
					<paper-button dialog-dismiss
							class="cancel-button"
							onclick="deleteBucketModal.close()">
						Cancel
					</paper-button>
				</div>
			</paper-dialog>
			<iron-ajax
					id="deleteBucketAjax"
					url="/balanceManagement/v1/bucket/"
					method="delete"
					on-response="_deleteBucketResponse"
					on-error="_deleteBucketError">
			</iron-ajax>
			<iron-ajax
					id="addBucketAjax"
					method = "post"
					content-type="application/json"
					on-loading-changed="_onLoadingChanged"
					on-response="_addBucketResponse"
					on-error="_addBucketError">
			</iron-ajax>
		`;
	}

	static get properties() {
		return {
			loading: {
				type: Boolean,
				value: false
			},
			productId: {
				type: String
			},
			bucketStart: {
				type: String
			},
			bucketEnd: {
				type: String
			},
			bucketAmount: {
				type: String
			},
			bucketUnit: {
				type: String
			},
			deleteBucketId: {
				type: String
			}
		}
	}

	ready() {
		super.ready()
	}

	_deleteBucket(event) {
		this.$.deleteBucketAjax.url = "/balanceManagement/v1/bucket/" + this.deleteBucketId;
		this.$.deleteBucketAjax.generateRequest();
	}

	_deleteBucketError(event) {
		this.$.deleteBucketModal.close();
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_deleteBucketResponse(event) {
		this.$.deleteBucketModal.close();
		this.deleteBucketId = null;
		document.body.querySelector('sig-app').shadowRoot.querySelector('sig-bucket-list').shadowRoot.getElementById('balanceBucketGrid').clearCache();
	}

	_bucketAddSubmit(event) {
		if(this.productId && this.bucketUnit && this.bucketAmount) {
			var bucket = {name: "channel"};
			bucket.lifecycleStatus = this.lifeStatus; 
			bucket.product = {id: this.productId,
					href: "/productInventoryManagement/v2/product/" + this.productId};
			if(this.bucketStart || this.bucketEnd) {
				bucket.validFor = new Object;
				if(this.bucketStart) {
					bucket.validFor.startDateTime = this.bucketStart;
				}
				if(this.bucketEnd) {
					bucket.validFor.endDateTime = this.bucketEnd;
				}
			}
			if(this.bucketUnit == "Bytes") {
				bucket.amount = {units: "octets", amount: this.bucketAmount};
			}
			else if(this.bucketUnit == "Cents") {
				bucket.amount = {units: "cents", amount: this.bucketAmount};
			}
			else if(this.bucketUnit == "Seconds") {
				bucket.amount = {units: "seconds", amount: this.bucketAmount};
			}
			var ajax = this.$.addBucketAjax;
			ajax.body = bucket;
			ajax.url="/balanceManagement/v1/product/" + this.productId + "/balanceTopup";
			ajax.generateRequest();
			this.$.addBucket.selected = null;
		}
	}

	_cancel() {
		this.productId = null;
		this.bucketStart = null;
		this.bucketEnd = null;
		this.bucketUnit = null;
		this.bucketAmount = null;
	}

	_addBucketResponse(event) {
		this.$.addBucketModal.close();
		this.productId = null;
		this.bucketStart = null;
		this.bucketEnd = null;
		this.bucketUnit = null;
		this.bucketAmount = null;
		document.body.querySelector('sig-app').shadowRoot.getElementById('bucketList').shadowRoot.getElementById('balanceBucketGrid').clearCache();
	}

	_addBucketError(event) {
		var toast = document.body.querySelector('sig-app').shadowRoot.getElementById('restError');
		toast.text = "Error";
		toast.open();
	}

	_onLoadingChanged(event) {
		if (this.$.addBucketAjax.loading) {
			this.$.progressId.disabled = false;
		} else {
			this.$.progressId.disabled = true;
		}
	}
}

window.customElements.define('sig-bucket-add', offerAdd);
